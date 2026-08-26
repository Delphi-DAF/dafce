unit Rtti.Feat;

{
  Lo que Daf.Rtti sabe decir de un generico CERRADO, que es el unico que existe en el
  binario: cuantos argumentos tiene y cuales. Y el marcador de argumento sin fijar, cuyo
  unico trabajo es que un T.Create escrito dentro de una implementacion sin fijar deje de
  ser silencioso.
}

interface

uses
  System.SysUtils,
  System.TypInfo,
  System.Generics.Collections;

type
  TThing = class
  end;

  IPair<T1, T2> = interface(IInvokable)
    ['{6C9B3C3F-1C4B-4B0E-9E2A-6A0E6F3B2D71}']
    function First: T1;
    function Second: T2;
  end;

  /// <summary>
  ///   El caso que de verdad prueba el troceo: el PRIMER argumento es a su vez generico, asi
  ///   que trae sus propias comas. Un troceo por comas a lo bruto cuenta tres argumentos donde
  ///   hay dos -- y con dos argumentos simples acierta por casualidad, que es por lo que ese
  ///   caso no puede ser el unico.
  /// </summary>
  INestedFirst = IPair<TDictionary<string, Integer>, TThing>;
  /// <summary>
  ///   El mismo anidamiento en la otra posicion, y falla de otra manera: aqui el troceo
  ///   ingenuo cuenta TRES argumentos donde hay dos. Hacen falta los dos casos porque cada
  ///   uno tumba una afirmacion distinta -- la posicion uno, el recuento el otro.
  /// </summary>
  INestedLast = IPair<TThing, TDictionary<string, Integer>>;
  ISimple = IPair<TThing, TThing>;

  /// <summary>
  ///   Escribe exactamente lo que escribiria una implementacion generica que intenta construir
  ///   su argumento. Instanciado sobre un cierre real construye lo suyo; instanciado sobre el
  ///   marcador tiene que protestar en vez de construir un marcador y seguir.
  /// </summary>
  TMaker<T: class, constructor> = class
  public
    class function Make: TObject;
  end;

  TRttiWorld = class
  public
    Arity: Integer;
    SecondArg: string;
    Outcome: string;
    Built: string;
    procedure Attempt(const Action: TProc);
  end;

const
  SinFallo = '<sin fallo>';

implementation

uses
  Daf.MiniSpec,
  Daf.Rtti;

{ TMaker<T> }

class function TMaker<T>.Make: TObject;
begin
  Result := T.Create;
end;

{ TRttiWorld }

procedure TRttiWorld.Attempt(const Action: TProc);
begin
  Outcome := SinFallo;
  try
    Action;
  except
    on E: Exception do Outcome := E.ClassName + ': ' + E.Message;
  end;
end;

function NameOfArg(const Args: TArray<PTypeInfo>; const Index: Integer): string;
begin
  if (Index < 0) or (Index > High(Args)) then Exit('<fuera de rango>');
  if Args[Index] = nil then Exit('<sin resolver>');
  Result := string(Args[Index].Name);
end;

initialization

Feature('''
Feature Reading a closed generic type @commons @rtti @generics

  As a framework author matching a generic contract against its instantiations
  I want the arguments of a closed generic read out of its type information
  So that two contracts of the same name are told apart by how many arguments they take
''')

.UseWorld<TRttiWorld>

.Rule('The arguments of a closed generic are counted respecting nesting')

  .Scenario('An argument that is itself generic counts as one')
    .When('the arguments of a closed generic whose first argument is generic are read',
      procedure(W: TRttiWorld)
      begin
        var Args := _T.GenericArgs<INestedFirst>;
        W.Arity := Length(Args);
        W.SecondArg := NameOfArg(Args, 1);
      end)
    .&Then('the second argument is the one that was written second',
      procedure(W: TRttiWorld)
      begin
        // Lo que se afirma es DONDE cayeron los cortes, no cuantos hubo. Medido: un troceo
        // por comas a lo bruto parte este nombre a mitad del primer argumento y aun asi
        // devuelve dos trozos -- el segundo es el Integer de dentro del diccionario.
        Expect(W.SecondArg).ToEqual('TThing');
      end)
    .&And('and there are two of them, which here is right even when the cuts are wrong',
      procedure(W: TRttiWorld)
      begin
        // Escrito para que se vea que no es esto lo que detecta nada en este caso.
        Expect(W.Arity).ToEqual(2);
      end)

  .Scenario('And when the generic argument goes last, the count is what gives it away')
    .When('the arguments of a closed generic whose last argument is generic are read',
      procedure(W: TRttiWorld)
      begin
        var Args := _T.GenericArgs<INestedLast>;
        W.Arity := Length(Args);
        W.SecondArg := NameOfArg(Args, 0);
      end)
    .&Then('the arity is the real one, not the number of commas',
      procedure(W: TRttiWorld)
      begin
        Expect(W.Arity).ToEqual(2);
      end)
    .&And('and the first argument is the one that was written first',
      procedure(W: TRttiWorld)
      begin
        Expect(W.SecondArg).ToEqual('TThing');
      end)

  .Scenario('Two plain arguments, which is the case that passes either way')
    // El control, y va escrito para que se vea que no prueba nada: con dos argumentos simples
    // el troceo ingenuo acierta. Esta aqui para que el escenario de arriba no se pueda
    // confundir con este.
    .When('the arguments of a closed generic with two plain arguments are read',
      procedure(W: TRttiWorld)
      begin
        var Args := _T.GenericArgs<ISimple>;
        W.Arity := Length(Args);
        W.SecondArg := NameOfArg(Args, 1);
      end)
    .&Then('there are two of them',
      procedure(W: TRttiWorld)
      begin
        Expect(W.Arity).ToEqual(2);
        Expect(W.SecondArg).ToEqual('TThing');
      end)

  .Scenario('A type that is not generic has no arguments')
    .When('the arguments of a plain class are read',
      procedure(W: TRttiWorld)
      begin
        W.Arity := Length(_T.GenericArgs(TypeInfo(TThing)));
        if _T.IsGeneric(TypeInfo(TThing)) then
          W.SecondArg := 'reported as generic'
        else
          W.SecondArg := 'not generic';
      end)
    .&Then('there are none, and it does not claim to be generic',
      procedure(W: TRttiWorld)
      begin
        Expect(W.Arity).ToEqual(0);
        Expect(W.SecondArg).ToEqual('not generic');
      end)

.Rule('The placeholder for an unbound argument is not a type to build')

  .Scenario('Building the placeholder aborts saying what happened')
    .When('a generic that builds its argument is instantiated over the placeholder',
      procedure(W: TRttiWorld)
      begin
        W.Attempt(
          procedure
          begin
            TMaker<_>.Make.Free;
          end);
      end)
    .&Then('it aborts instead of building a placeholder and carrying on',
      procedure(W: TRttiWorld)
      begin
        Expect(W.Outcome).ToContain('placeholder');
      end)
    .&And('and it says where to read the concrete class instead',
      procedure(W: TRttiWorld)
      begin
        Expect(W.Outcome).ToContain('_ClosedArgs');
      end)

  .Scenario('The same code over a real closure still builds its own')
    // Sin este caso la escena de arriba pasaria con un marcador que rompiese SIEMPRE, y lo
    // que hay que probar es que solo rompe donde no habia nada que construir.
    .When('the same generic is instantiated over a real class',
      procedure(W: TRttiWorld)
      begin
        W.Attempt(
          procedure
          begin
            var Obj := TMaker<TThing>.Make;
            try
              W.Built := Obj.ClassName;
            finally
              Obj.Free;
            end;
          end);
      end)
    .&Then('it builds what it was asked for',
      procedure(W: TRttiWorld)
      begin
        Expect(W.Built).ToEqual('TThing');
      end)
    .&And('and nothing was raised on the way',
      procedure(W: TRttiWorld)
      begin
        Expect(W.Outcome).ToEqual(SinFallo);
      end);

end.
