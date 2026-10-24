unit Daf.DependencyInjection.OpenGenerics;

{
  Registrar un contrato generico UNA vez y servir todos sus cierres.

  Delphi cierra los genericos al compilar: TX<TCierre> para un TCierre que el compilador
  nunca vio no esta en el binario, asi que el contenedor no puede fabricarlo. Lo que si se
  puede es registrar la implementacion instanciada sobre un MARCADOR --el `_` de Daf.Rtti--
  y entregar ese unico objeto para cualquier cierre: IX<_> es una instanciacion cerrada
  legitima con su propio tipo, todas las instanciaciones comparten el GUID declarado, y con
  los argumentos viajando como referencia el codigo maquina es el mismo.

  Ese "con los argumentos viajando como referencia" es una precondicion, no una esperanza, y
  aqui esta lo que la convierte en diseño: al resolver estan los DOS contratos delante y los
  dos describen sus metodos, asi que se comparan uno a uno. Un cierre que no cumpla se
  rechaza nombrando el metodo. Sin esa comprobacion el caso invalido no da error: corrompe la
  pila en silencio.

  El precio esta escrito en el README: dentro de TX<_>, T ES el marcador. Lo que la
  implementacion necesite del tipo real le llega por el campo _ClosedArgs, que el contenedor
  rellena ANTES de que corra el constructor. Declararlo es de quien implementa: la que no
  necesita nada del cierre no lo declara y no paga nada por ello.
}

interface

uses
  System.SysUtils,
  System.TypInfo,
  System.Rtti,
  Daf.Extensions.DependencyInjection;

type
  /// <summary>
  ///   Un registro sin fijar que no se va a poder cumplir. Se levanta en dos momentos, y la
  ///   diferencia importa: al REGISTRAR cuando la implementacion declara donde recibir los
  ///   tipos del cierre de una forma que el contenedor no puede rellenar --eso es un error de
  ///   composicion y se dice al componer--, y al RESOLVER cuando el cierre concreto no encaja
  ///   con lo que el registro puede servir.
  /// </summary>
  EOpenGenericError = class(EServiceProviderError)
  end;

  TOpenGeneric = record
  strict private
    class function ArgFields(const AClass: TClass; out Holder: TRttiField): TArray<TRttiField>; static;
    class function SameRepresentation(const A, B: PTypeInfo): Boolean; static;
    class function Verdict(const OpenType, ClosedType: PTypeInfo): string; static;
    class function Incompatible(const OpenType, ClosedType: PTypeInfo; const MethodName, Where: string;
      const A, B: PTypeInfo): string; static;
  public
    /// <summary>El campo de nombre fijo por el que la implementacion recibe los tipos del
    /// cierre. Lo declara quien implementa --y solo si le hace falta-- y lo escribe el
    /// contenedor: se lee libremente y no se asigna nunca. Su visibilidad da igual: las
    /// directivas RTTI por defecto acotan a public/published los metodos y las propiedades,
    /// no los campos.</summary>
    const ClosedArgs = '_ClosedArgs';

    /// <summary>Un tipo de servicio que deja sus argumentos sin fijar, o sea que alguno de
    /// ellos es el marcador.</summary>
    class function IsOpen(const PInfo: PTypeInfo): Boolean; static;

    /// <summary>Si un registro sin fijar puede servir a un cierre: mismo nombre y misma
    /// ARIDAD. El nombre no basta por si solo -- dos contratos pueden llamarse igual y
    /// distinguirse solo por cuantos argumentos toman.
    /// Su limite, que va escrito en el README: dos familias del MISMO nombre y la MISMA
    /// aridad, declaradas en units distintas, se emparejan. Lo que queda debajo no es la
    /// nada -- CheckMatch compara los metodos y rechaza el emparejamiento nombrando el
    /// primero que no encaja -- pero es una red, no una comprobacion de identidad.</summary>
    class function SameForm(const OpenType, ClosedType: PTypeInfo): Boolean; static;

    /// <summary>Que la implementacion pueda cumplir lo que el registro promete. Se llama AL
    /// REGISTRAR: un registro imposible es un error de composicion, y sin esto el fallo seria
    /// mudo y tardio -- el contenedor no podria rellenar lo que la implementacion declaro, ella
    /// leeria un hueco vacio y reventaria al usarlo, lejos del registro que lo causo.
    /// No exige que el campo exista: la implementacion que no necesita nada del cierre no
    /// declara nada, y no hay hueco vacio que prevenir.</summary>
    class procedure CheckImplementation(const OpenType: PTypeInfo; const AClass: TClass); static;

    /// <summary>Que entregar la implementacion del registro sin fijar para este cierre sea
    /// seguro. Una vez por par y cacheado: recorrer los metodos de dos interfaces en cada
    /// resolucion seria pagar por llamada, que es justo lo que este diseño evita.</summary>
    /// <remarks>
    ///   Se llama al EMPAREJAR, no al construir, y por eso no esta dentro de Activate: que un
    ///   emparejamiento sea seguro es una propiedad del par contrato-abierto/cierre, y no
    ///   depende de si el objeto lo fabrica una clase o una factory escrita a mano.
    /// </remarks>
    class procedure CheckMatch(const OpenType, ClosedType: PTypeInfo); static;

    /// <summary>Lee los tipos del cierre, comprueba las cotas, rellena y construye, en ese
    /// orden -- y las tres primeras cosas solo cuando la implementacion declaro donde
    /// recibirlos. La seguridad del emparejamiento ya la ha comprobado CheckMatch antes de
    /// llegar aqui, y esa no depende de lo que la implementacion declare.</summary>
    class function Activate(const Provider: IServiceProvider; const AClass: TClass;
      const OpenType, ClosedType: PTypeInfo): TObject; static;
  end;

implementation

uses
  System.Generics.Collections,
  Daf.Rtti,
  Daf.DependencyInjection.ActivatorUtilities;

var
  RC: TRttiContext;
  // Cadena vacia = el par es compatible; cualquier otra cosa ES el mensaje que se levanta.
  // Guardar el veredicto y no un booleano es lo que permite cachear tambien el rechazo sin
  // perder lo unico que lo hace util, que es decir QUE metodo no encaja.
  GVerdicts: TDictionary<string, string>;
  GVerdictsLock: TObject;

function PairKey(const OpenType, ClosedType: PTypeInfo): string;
begin
  Result := Format('%p|%p', [Pointer(OpenType), Pointer(ClosedType)]);
end;

function KindName(const PInfo: PTypeInfo): string;
begin
  if PInfo = nil then Exit('nothing');
  Result := Format('%s (%s)', [string(PInfo.Name), TEnum.ToString<TTypeKind>(PInfo.Kind)]);
end;

{ TOpenGeneric }

class function TOpenGeneric.IsOpen(const PInfo: PTypeInfo): Boolean;
begin
  Result := False;
  if not _T.IsGeneric(PInfo) then Exit;
  for var Arg in _T.GenericArgs(PInfo) do
    if Arg = System.TypeInfo(_) then Exit(True);
end;

class function TOpenGeneric.SameForm(const OpenType, ClosedType: PTypeInfo): Boolean;
begin
  Result := False;
  if (OpenType = nil) or (ClosedType = nil) then Exit;
  if OpenType.Kind <> ClosedType.Kind then Exit;

  var Family := _T.GenericName(OpenType);
  if Family = '' then Exit;
  if not SameText(Family, _T.GenericName(ClosedType)) then Exit;

  // Y la aridad, contada respetando el anidamiento: el prefijo hasta '<' no separa IFoo<_>
  // de IFoo<_,_>.
  Result := Length(_T.GenericArgs(OpenType)) = Length(_T.GenericArgs(ClosedType));
end;

class function TOpenGeneric.ArgFields(const AClass: TClass; out Holder: TRttiField): TArray<TRttiField>;
begin
  Result := nil;
  Holder := nil;
  if AClass = nil then Exit;
  var RT := RC.GetType(AClass);
  if RT = nil then Exit;
  // Campo de INSTANCIA, y no un class var: la RTTI no expone los class var, y aunque los
  // expusiera TX<_> es una sola clase, asi que todos los cierres compartirian el valor y el
  // segundo pisaria al primero.
  // La visibilidad no entra aqui, y no por descuido: las directivas RTTI por defecto acotan a
  // public/published los metodos y las propiedades, para los CAMPOS incluyen las cuatro. Asi
  // que GetField lo encuentra tambien en strict private.
  Holder := RT.GetField(ClosedArgs);
  if Holder = nil then Exit;
  // Holder sale puesto aunque el campo no sea un record, que es lo que permite a quien llama
  // distinguir "no lo declara" de "lo declara y no se puede rellenar".
  if Holder.FieldType is TRttiRecordType then
    Result := Holder.FieldType.GetFields;
end;

class procedure TOpenGeneric.CheckImplementation(const OpenType: PTypeInfo; const AClass: TClass);
begin
  var Holder: TRttiField;
  var Fields := ArgFields(AClass, Holder);

  // No declararla no es un error: equivale a declararla con una posicion por argumento y todas
  // de tipo TClass -- nada que rellenar y ninguna cota que comprobar. Lo que sigue siendo un
  // error de composicion es declararla de una forma que no se pueda rellenar.
  if Holder = nil then Exit;

  if not (Holder.FieldType is TRttiRecordType) then
  begin
    var What := 'of a type the RTTI does not describe';
    if Holder.FieldType <> nil then
      What := string(Holder.FieldType.Name);
    raise EOpenGenericError.CreateFmt(
      'Cannot register %s for the open service %s: %s is %s, and it has to be a record with one ' +
      'class-reference field per type argument -- that record is where the container writes the ' +
      'closed type arguments. An implementation that needs nothing from the closure does not ' +
      'declare %s at all.',
      [AClass.ClassName, _T.NameOf(OpenType), ClosedArgs, What, ClosedArgs]);
  end;

  var Arity := Length(_T.GenericArgs(OpenType));
  if Length(Fields) <> Arity then
    raise EOpenGenericError.CreateFmt(
      'Cannot register %s for the open service %s: %s declares %d field(s) and the service takes ' +
      '%d type argument(s). The container binds them BY POSITION -- Delphi emits nothing about the ' +
      'names of a generic''s parameters -- so the two have to agree in number.',
      [AClass.ClassName, _T.NameOf(OpenType), ClosedArgs, Length(Fields), Arity]);

  for var Field in Fields do
    if not (Field.FieldType is TRttiClassRefType) then
      raise EOpenGenericError.CreateFmt(
        'Cannot register %s for the open service %s: field %s.%s is %s. Every field of %s has to be ' +
        'a class reference -- "class of TSomething" to declare a bound, TClass for none -- because ' +
        'that type IS how the implementation says what it can accept.',
        [AClass.ClassName, _T.NameOf(OpenType), ClosedArgs, Field.Name, Field.FieldType.Name, ClosedArgs]);
end;

class function TOpenGeneric.SameRepresentation(const A, B: PTypeInfo): Boolean;
const
  // Lo que viaja como referencia: un puntero, pasado y devuelto igual sea cual sea el tipo.
  // Fuera de aqui la representacion depende del tipo -- un Integer, un string por resultado,
  // un record -- y el emparejamiento dejaria de ser una equivalencia para ser una mentira.
  ByReference = [tkClass, tkInterface, tkClassRef, tkPointer];
begin
  if A = B then Exit(True);
  if (A = nil) or (B = nil) then Exit(False);
  Result := (A.Kind = B.Kind) and (A.Kind in ByReference);
end;

class function TOpenGeneric.Incompatible(const OpenType, ClosedType: PTypeInfo;
  const MethodName, Where: string; const A, B: PTypeInfo): string;
begin
  Result := Format(
    'Cannot serve %s from the open registration of %s: method %s does not have the same call ' +
    'shape in both -- %s is %s in the open form and %s in the closed one. An argument that ' +
    'travels by value changes how the call is made, so entering this match would corrupt the ' +
    'stack instead of failing.',
    [_T.NameOf(ClosedType), _T.NameOf(OpenType), MethodName, Where, KindName(A), KindName(B)]);
end;

class function TOpenGeneric.Verdict(const OpenType, ClosedType: PTypeInfo): string;

  function HandleOf(const RType: TRttiType): PTypeInfo;
  begin
    if RType = nil then Result := nil else Result := RType.Handle;
  end;

begin
  Result := '';
  var OpenRT := RC.GetType(OpenType);
  var ClosedRT := RC.GetType(ClosedType);
  if not ((OpenRT is TRttiInterfaceType) and (ClosedRT is TRttiInterfaceType)) then
    Exit(Format('Cannot serve %s from the open registration of %s: only interfaces can be ' +
      'registered with their type arguments left open.', [_T.NameOf(ClosedType), _T.NameOf(OpenType)]));

  // Sin RTTI de metodos no hay nada que comparar, y "nada que comparar" no es "compatible":
  // seria un verde que coincide con su modo de fallo. Derivar de IInvokable es lo que la trae.
  if not ((ifHasRtti in TRttiInterfaceType(OpenRT).IntfFlags) and
          (ifHasRtti in TRttiInterfaceType(ClosedRT).IntfFlags)) then
    Exit(Format('Cannot serve %s from the open registration of %s: neither carries method type ' +
      'information, so the container cannot check that the two are call-compatible. Derive the ' +
      'contract from IInvokable.', [_T.NameOf(ClosedType), _T.NameOf(OpenType)]));

  var OpenMethods := OpenRT.GetMethods;
  var ClosedMethods := ClosedRT.GetMethods;
  if Length(OpenMethods) <> Length(ClosedMethods) then
    Exit(Format('Cannot serve %s from the open registration of %s: they describe %d and %d ' +
      'methods.', [_T.NameOf(ClosedType), _T.NameOf(OpenType), Length(OpenMethods), Length(ClosedMethods)]));

  for var Idx := 0 to High(OpenMethods) do
  begin
    var Open := OpenMethods[Idx];
    var Closed := ClosedMethods[Idx];

    if not SameRepresentation(HandleOf(Open.ReturnType), HandleOf(Closed.ReturnType)) then
      Exit(Incompatible(OpenType, ClosedType, Open.Name, 'its result',
        HandleOf(Open.ReturnType), HandleOf(Closed.ReturnType)));

    var OpenParams := Open.GetParameters;
    var ClosedParams := Closed.GetParameters;
    if Length(OpenParams) <> Length(ClosedParams) then
      Exit(Format('Cannot serve %s from the open registration of %s: method %s takes %d and %d ' +
        'parameters.', [_T.NameOf(ClosedType), _T.NameOf(OpenType), Open.Name,
        Length(OpenParams), Length(ClosedParams)]));

    for var P := 0 to High(OpenParams) do
    begin
      if OpenParams[P].Flags <> ClosedParams[P].Flags then
        Exit(Incompatible(OpenType, ClosedType, Open.Name, 'parameter "' + OpenParams[P].Name + '"',
          HandleOf(OpenParams[P].ParamType), HandleOf(ClosedParams[P].ParamType)));
      if not SameRepresentation(HandleOf(OpenParams[P].ParamType), HandleOf(ClosedParams[P].ParamType)) then
        Exit(Incompatible(OpenType, ClosedType, Open.Name, 'parameter "' + OpenParams[P].Name + '"',
          HandleOf(OpenParams[P].ParamType), HandleOf(ClosedParams[P].ParamType)));
    end;
  end;
end;

class procedure TOpenGeneric.CheckMatch(const OpenType, ClosedType: PTypeInfo);
begin
  var Key := PairKey(OpenType, ClosedType);
  var Answer: string;
  TMonitor.Enter(GVerdictsLock);
  try
    if not GVerdicts.TryGetValue(Key, Answer) then
    begin
      Answer := Verdict(OpenType, ClosedType);
      GVerdicts.Add(Key, Answer);
    end;
  finally
    TMonitor.Exit(GVerdictsLock);
  end;
  if Answer <> '' then
    raise EOpenGenericError.Create(Answer);
end;

class function TOpenGeneric.Activate(const Provider: IServiceProvider; const AClass: TClass;
  const OpenType, ClosedType: PTypeInfo): TObject;
begin
  // Los tipos del cierre, y que cumplan la cota que la implementacion declara. La cota se
  //    lee del TIPO DEL CAMPO: "class of TEntity" dice "solo entidades", TClass dice "lo que
  //    sea". Delphi no emite nada sobre las restricciones de un generico, asi que en el
  //    camino sin fijar manda el record.
  var Holder: TRttiField;
  var Fields := ArgFields(AClass, Holder);

  // Sin via de escape no hay cotas que verificar ni nada que rellenar, y eso NO es dejar de
  // comprobar: es que no hay nada que comprobar. Lo que protege la entrega aqui es la
  // compatibilidad de las firmas, que ya ha mirado CheckMatch y no depende de lo que la
  // implementacion declare.
  if Length(Fields) = 0 then
    Exit(TActivatorUtilities.CreateUnwired(Provider, AClass));

  var Args := _T.GenericArgs(ClosedType);
  var Classes: TArray<TClass>;
  SetLength(Classes, Length(Fields));
  for var Idx := 0 to High(Fields) do
  begin
    var Arg := Args[Idx];
    if (Arg = nil) or (Arg.Kind <> tkClass) then
      raise EOpenGenericError.CreateFmt(
        'Cannot serve %s from the open registration of %s: type argument %d is not a class the ' +
        'container can hand over as a value. The placeholder is a class, so every argument has to be one.',
        [_T.NameOf(ClosedType), _T.NameOf(OpenType), Idx + 1]);

    var ArgClass := Arg.TypeData.ClassType;
    var Bound := TRttiClassRefType(Fields[Idx].FieldType).MetaclassType;
    if not ArgClass.InheritsFrom(Bound) then
      raise EOpenGenericError.CreateFmt(
        'Cannot serve %s from the open registration of %s: type argument %d is %s, and %s.%s ' +
        'declares it has to be a %s.',
        [_T.NameOf(ClosedType), _T.NameOf(OpenType), Idx + 1, ArgClass.ClassName,
         ClosedArgs, Fields[Idx].Name, Bound.ClassName]);
    Classes[Idx] := ArgClass;
  end;

  // Rellenar ANTES de que corra el constructor, que es donde se decide casi todo: un
  //    registrador elige su categoria al construirse, no despues.
  Result := TActivatorUtilities.CreateUnwired(Provider, AClass,
    procedure(Instance: TObject)
    begin
      // El campo del record vive dentro del campo de la instancia: la base para escribirlo es
      // la instancia desplazada hasta el record.
      var Base: Pointer := PByte(Instance) + Holder.Offset;
      for var Idx := 0 to High(Fields) do
      begin
        var Value: TValue;
        TValue.Make(@Classes[Idx], Fields[Idx].FieldType.Handle, Value);
        Fields[Idx].SetValue(Base, Value);
      end;
    end);
end;

initialization
  RC := TRttiContext.Create;
  GVerdicts := TDictionary<string, string>.Create;
  GVerdictsLock := TObject.Create;

finalization
  GVerdictsLock.Free;
  GVerdicts.Free;
  RC.Free;

end.
