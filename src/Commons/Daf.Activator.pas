unit Daf.Activator;

interface

uses
  System.Rtti,
  System.TypInfo,
  System.Classes;

type
  TActivator = class
  private
    class function ArgumentsMatch(AMethod: TRttiMethod; const AArgs: array of TValue): Boolean;
    class function FindMatchingCtor(AClass: TClass; const AArgs: array of TValue): TRttiMethod; overload;
  public
    class function CreateInstance<T: class>: T; overload;
    class function CreateInstance<T: class>(const Args: array of TValue): T; overload;
    class function CreateInstance(AClass: TClass): TObject; overload;
    class function CreateInstance(AClass: TClass; const Args: array of TValue): TObject; overload;
    class function CreateInstance(ATypeInfo: PTypeInfo): TObject; overload;
    class function CreateInstance(ATypeName: string): TObject; overload;
    class function CreateInstance(ATypeInfo: PTypeInfo; const AArgs: array of TValue): TObject; overload;
    class function CreateInstance(ATypeName: string; const AArgs: array of TValue): TObject; overload;
  end;

implementation

{ TActivator }

class function TActivator.ArgumentsMatch(AMethod: TRttiMethod; const AArgs: array of TValue): Boolean;
var
  Arg: TValue;
begin
  var
  Params := AMethod.GetParameters;
  if Length(Params) <> Length(AArgs) then
    Exit(False);
  for var idx := 0 to High(Params) do
  begin
    var
    ParamType := Params[idx].ParamType;
    if ParamType.Handle = AArgs[idx].TypeInfo then
      Continue;
    if not AArgs[idx].TryCast(ParamType.Handle, Arg) then
      Exit(False);
  end;
  Result := True;
end;

class function TActivator.FindMatchingCtor(AClass: TClass; const AArgs: array of TValue): TRttiMethod;
var
  RType: TRttiType;
begin
  Result := nil;
  var RC := TRttiContext.Create;
  try
    RType := RC.GetType(AClass);
    for var Candidate in RType.GetMethods do
    begin
      if not(Candidate.IsConstructor) then
        Continue;
      if not ArgumentsMatch(Candidate, AArgs) then
        Continue;
      Result := Candidate;
      if Result.Name = 'Create' then
        Exit;
    end;
  finally
    RC.Free;
  end;
end;

class function TActivator.CreateInstance<T>: T;
begin
  Result := CreateInstance<T>([]);
end;

class function TActivator.CreateInstance<T>(const Args: array of TValue): T;
begin
  Result := CreateInstance(T, Args) as T;
end;

class function TActivator.CreateInstance(AClass: TClass): TObject;
begin
  Result :=  CreateInstance(AClass, []);
end;

class function TActivator.CreateInstance(AClass: TClass; const Args: array of TValue): TObject;
begin
  var
  MatchingCtor := FindMatchingCtor(AClass, Args);
  Result := MatchingCtor.Invoke(AClass, Args).AsObject;
end;

class function TActivator.CreateInstance(ATypeInfo: PTypeInfo): TObject;
begin
  Result := CreateInstance(ATypeInfo, []);
end;

class function TActivator.CreateInstance(ATypeInfo: PTypeInfo; const AArgs: array of TValue): TObject;
begin
  var RC := TRttiContext.Create;
  try
    var
    AClass := TRttiInstanceType(RC.GetType(ATypeInfo)).MetaclassType;
    Result := CreateInstance(AClass, AArgs);
  finally
    RC.Free;
  end;
end;

class function TActivator.CreateInstance(ATypeName: string): TObject;
begin
  Result := CreateInstance(ATypeName, []);
end;

class function TActivator.CreateInstance(ATypeName: string; const AArgs: array of TValue): TObject;
begin
  Result := CreateInstance(ATypeName, AArgs);
end;

end.
