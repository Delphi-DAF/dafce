unit ConsoleSample.Greeter;

interface

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
    // Como alternativa se puede requerir directamente IConfiguration
    constructor Create(const ATitle, AVersion, AMessage: string);
    procedure Greet;
  end;

  IWrapperImpl = interface(IInvokable)
    function Alive: Boolean;
  end;

  TWrapper = class(TInterfacedObject, IWrapperImpl)
  public
    function Alive: Boolean;
  end;

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

  TWrapperAccessor = class
  private
    FWrapper: IWrapper;
  public
    // Como alternativa se puede requerir directamente IConfiguration
    constructor Create(Wrapper: IWrapper);
    function ChecWrapperAccess: Boolean;
    property Wrapper: IWrapper read FWrapper;
  end;

implementation

uses
  System.SysUtils;

{ TGreeter }

constructor TGreeter.Create(const ATitle, AVersion, AMessage: string);
begin
  FTitle := ATitle;
  FVersion := AVersion;
  FMessage := AMessage;
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