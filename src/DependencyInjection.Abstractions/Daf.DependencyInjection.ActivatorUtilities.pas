unit Daf.DependencyInjection.ActivatorUtilities;

interface

uses
  System.Rtti,
  System.TypInfo,
  System.Classes,
  Daf.Extensions.DependencyInjection,
  Daf.Activator;

type

  ActivatorUtilitiesConstructorAttribute = class(TCustomAttribute)
  end;

  TActivatorUtilities = class
  private
    class function TryResolveArgs(Provider: IServiceProvider; Ctor: TRttiMethod; out Args: TArray<TValue>; out Failed: TRttiParameter): Boolean;
    class function GetResolvableCtors(const AClass: TClass; const Provider: IServiceProviderImpl = nil): TArray<TRttiMethod>;
    class function SelectBest(Ctors: TArray<TRttiMethod>): TRttiMethod;
  public
    class function CanResolveArgs(const Provider: IServiceProvider; const Ctor: TRttiMethod): Boolean;overload;
    class function CanResolveArgs(const Provider: IServiceProvider; const Ctor: TRttiMethod; out Failed: TRttiParameter): Boolean;overload;
    /// <summary>
    /// Responde a la misma pregunta que CreateInstance se hace al construir: hay algun
    /// constructor que el contenedor pueda satisfacer? Cuando no lo hay, devuelve el mejor
    /// candidato y el primer parametro que nadie registra, para poder decirlo en el mensaje.
    /// </summary>
    class function TryFindConstructor(const Provider: IServiceProvider; const AClass: TClass; out Ctor: TRttiMethod;
      out Failed: TRttiParameter): Boolean;
    class function CreateInstance(const Provider: IServiceProvider; const AClass: TClass): TObject; overload;
    class function CreateInstance<T: class>(const Provider: IServiceProvider): T; overload;
  end;

implementation

uses
  System.SysUtils,
  Daf.Types;

var RC: TRttiContext;


{ TActivatorUtilities }

class function TActivatorUtilities.CanResolveArgs(const Provider: IServiceProvider; const Ctor: TRttiMethod): Boolean;
begin
  var Failed: TRttiParameter;
  Result := CanResolveArgs(Provider,Ctor, Failed);
end;

class function TActivatorUtilities.CanResolveArgs(const Provider: IServiceProvider; const Ctor: TRttiMethod; out Failed: TRttiParameter): Boolean;
begin
  Failed := nil;
  Result := True;
  var Params := Ctor.GetParameters;
  for var idx := 0 to Length(Params) - 1 do
  begin
    var PInfo := Params[idx].ParamType.Handle;
    if (PInfo = TypeInfo(IServiceProviderImpl)) then Continue;
    if (PInfo = TypeInfo(IServiceProvider)) then Continue;
    if (PInfo.Kind in [tkInterface, tkClass]) and not Provider.CanResolve(PInfo) then
    begin
      Failed := Params[idx];
      Exit(False);
    end;
  end;
end;

class function TActivatorUtilities.GetResolvableCtors(const AClass: TClass; const Provider: IServiceProviderImpl): TArray<TRttiMethod>;
begin
  Result := [];
  if AClass = nil then Exit;
  var RType := RC.GetType(AClass) ;
  for var Method in RType.GetDeclaredMethods do
  begin
    if not Method.IsConstructor then Continue;
    if not (Method.Visibility in [mvPublic, mvPublished]) then Continue;
    if Assigned(Provider) and not CanResolveArgs(Provider, Method) then Continue;
    if Method.GetAttribute<ActivatorUtilitiesConstructorAttribute> <> nil then
    begin
      Result := [Method];
      Exit;
    End;
    Result := Result + [Method];
  end;
  if Length(Result) = 0 then
    Result := GetResolvableCtors(AClass.ClassParent, Provider)
end;

class function TActivatorUtilities.SelectBest(Ctors: TArray<TRttiMethod>): TRttiMethod;
begin
  Result := nil;
  if Length(Ctors) = 0 then Exit;
  var Best := Ctors[0];
  for var idx := 1 to Length(Ctors) - 1 do
    if Length(Ctors[idx].GetParameters) > Length(Best.GetParameters) then
      Best := Ctors[idx];
  Result := Best;
end;

class function TActivatorUtilities.TryFindConstructor(const Provider: IServiceProvider; const AClass: TClass;
  out Ctor: TRttiMethod; out Failed: TRttiParameter): Boolean;
begin
  Failed := nil;
  Ctor := nil;
  // Candidatos: los constructores publicos del primer nivel de la jerarquia que declara
  // alguno. Aqui NO se cae al constructor de un ancestro cuando ninguno de los propios se
  // puede satisfacer, y esa es justo la diferencia con CreateInstance: una clase que pide
  // una dependencia por constructor y acaba construida con TObject.Create queda a medias,
  // y ese es el fallo que la validacion existe para ver.
  var Declared := GetResolvableCtors(AClass);
  if Length(Declared) = 0 then Exit(True);

  // Que la clase declare mas de un constructor no es un fallo: basta con que alguno se
  // pueda satisfacer, como hace el activador al elegir.
  var Resolvable: TArray<TRttiMethod> := [];
  for var Candidate in Declared do
    if CanResolveArgs(Provider, Candidate) then
      Resolvable := Resolvable + [Candidate];

  Ctor := SelectBest(Resolvable);
  Result := Ctor <> nil;
  if Result then Exit;

  Ctor := SelectBest(Declared);
  if Ctor <> nil then
    CanResolveArgs(Provider, Ctor, Failed);
end;

class function TActivatorUtilities.TryResolveArgs(Provider: IServiceProvider; Ctor: TRttiMethod; out Args: TArray<TValue>; out Failed: TRttiParameter): Boolean;
begin
  Result := True;
  var Params := Ctor.GetParameters;
  SetLength(Args, Length(Params));
  for var idx := 0 to Length(Params) - 1 do
  begin
    var PInfo := Params[idx].ParamType.Handle;
    if not (Pinfo.Kind in [tkInterface, tkClass, tkRecord]) then Continue;

    var Aux: IInterface;
    if (PInfo = TypeInfo(IServiceProviderImpl)) or (PInfo = TypeInfo(IServiceProvider)) then
      Aux := Provider
    else
    if not Provider.TryGet(PInfo, Aux) and (Pinfo.Kind <> tkRecord) then
    begin
      Failed := Params[idx];
      Exit(False);
    end;
    TValue.Make(@Aux, PInfo, Args[idx]);
  end;
end;

class function TActivatorUtilities.CreateInstance(const Provider: IServiceProvider; const AClass: TClass): TObject;
begin
  var Ctor := SelectBest(GetResolvableCtors(AClass, Provider));
  if Ctor = nil then
    raise EArgumentException.CreateFmt('Cannot find resolvable constructor for %s', [AClass.ClassName]);

  var Args: TArray<TValue>;
  var Failed: TRttiParameter;
  if not TryResolveArgs(Provider, Ctor, Args, Failed) then
    raise EArgumentException.CreateFmt('Cannot resolve argument %s for %s', [Failed.Name, AClass.ClassName]);
  Result := Ctor.Invoke(AClass, Args).AsObject
end;

class function TActivatorUtilities.CreateInstance<T>(const Provider: IServiceProvider): T;
begin
  Result := CreateInstance(Provider, T) as T;
end;

initialization
  RC := TRttiContext.Create;
finalization
  RC.Free;
end.
