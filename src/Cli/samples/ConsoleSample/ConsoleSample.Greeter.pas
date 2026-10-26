unit ConsoleSample.Greeter;

interface

uses
  Daf.Extensions.Options,
  ConsoleSample.AppSettings;

type
  // los servicios inyectados deben tener GUID (para identifcar su tipo) y RTTI
  // Para que tengan RTTI, basta extender IInvokable, o activarla en el tipo
  // con la directiva $M+ / $M-.
  IGreeter = interface(IInvokable)
    ['{10B734DC-FADF-4E31-9DE4-76E3BFC27D1D}']
    procedure Greet;
  end;

  TGreeter = class(TInterfacedObject, IGreeter)
  private
    FTitle: string;
    FVersion: string;
    FMessage: string;
  public
    // Pide sus ajustes por constructor, como cualquier otra dependencia: aqui no aparece de
    // donde salen sus valores. Lo decide quien compone, y por eso este servicio no nombra
    // ninguna seccion de configuracion ni libera nada.
    //
    // Y COPIA lo que necesita en vez de guardarse el objeto: Value devuelve un objeto crudo
    // cuyo dueño es la opcion, asi que quedarselo y soltar la opcion dejaria un puntero
    // colgando. La otra forma correcta es sostener la IOptions<T> en un campo.
    constructor Create(const Options: IOptions<TAppSettings>);
    procedure Greet;
  end;

  {
    LO QUE ESTE TRIO DEMUESTRA:

    un parametro de constructor puede ser un RECORD que nadie registra en el contenedor, si el
    record declara una conversion implicita desde una interfaz que si esta registrada. Aqui
    TWrapperAccessor pide un IWrapper --el record--, nadie registra IWrapper, y el contenedor lo
    construye resolviendo IWrapperImpl y aplicando IWrapper.Implicit.

    De ahi sale el resto del diseño, que sin esto parece decoracion: los operadores Equal y
    NotEqual contra Pointer existen para poder escribir `Wrapper <> nil` sobre un record que se
    comporta como una referencia de interfaz. Es el mismo patron con el que DAF envuelve su propio
    IServiceProvider.
  }
  IWrapperImpl = interface(IInvokable)
    function Alive: Boolean;
  end;

  TWrapper = class(TInterfacedObject, IWrapperImpl)
  public
    function Alive: Boolean;
  end;

  // Prefijamos con I en vez de T porque su propósito es servir como interfaz a todos los efectos
  IWrapper = record
  private
    FImpl: IWrapperImpl;
  public
    class operator Implicit(Impl: IWrapperImpl): IWrapper;
    class operator Implicit(Value: IWrapper): IWrapperImpl;
    class operator Equal(Value: IWrapper; P: Pointer): Boolean;
    class operator NotEqual(Value: IWrapper; P: Pointer): Boolean;
    function Alive: Boolean;inline;
  end;

  // Una clase que depende de IWrapper en su constructor para demostrar
  // que el contenedor de dependencias lo resuelve
  TWrapperAccessor = class
  private
    FWrapper: IWrapper;
  public
    constructor Create(Wrapper: IWrapper);
    function ChecWrapperAccess: Boolean;
    property Wrapper: IWrapper read FWrapper;
  end;

implementation

uses
  System.SysUtils;

{ TGreeter }

constructor TGreeter.Create(const Options: IOptions<TAppSettings>);
begin
  var Settings := Options.Value;
  FTitle := Settings.Title;
  FVersion := Settings.Version;
  FMessage := Settings.Message;
end;

procedure TGreeter.Greet;
begin
  Writeln(StringOfChar('=', 30));
  Writeln(Format('  %s v%s', [FTitle, FVersion]));
  Writeln(StringOfChar('=', 30));
  Writeln(FMessage);
end;

{ IWrapper }

function IWrapper.Alive: Boolean;
begin
  Result := FImpl.Alive;
end;

class operator IWrapper.Implicit(Impl: IWrapperImpl): IWrapper;
begin
  Result.FImpl := Impl;
end;

class operator IWrapper.Implicit(Value: IWrapper): IWrapperImpl;
begin
  Result := Value.FImpl;
end;

class operator IWrapper.Equal(Value: IWrapper; P: Pointer): Boolean;
begin
  Result := Pointer(Value.FImpl) = P;
end;

class operator IWrapper.NotEqual(Value: IWrapper; P: Pointer): Boolean;
begin
  Result := Pointer(Value.FImpl) <> P;
end;

{ TWrapper }

function TWrapper.Alive: Boolean;
begin
  Result := True;
end;

{ TWrapperAcessor }

function TWrapperAccessor.ChecWrapperAccess: Boolean;
begin
  Result := (Wrapper <> nil) and Wrapper.Alive;
end;

constructor TWrapperAccessor.Create(Wrapper: IWrapper);
begin
  inherited Create;
  FWrapper := Wrapper;
end;

end.
