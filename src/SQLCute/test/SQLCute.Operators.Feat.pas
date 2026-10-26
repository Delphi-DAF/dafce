unit SQLCute.Operators.Feat;

{
  La tabla de operadores textuales: la cadena que escribe quien llama --Where(col, '<=', v)--
  y el operador del arbol al que se traduce.

  Dos afirmaciones que no son entradas de la tabla y estan aqui a proposito: que la lectura no
  distingue mayusculas y que recorta los espacios. Las dos son contrato para quien escribe la cadena.
}

interface

implementation

uses
  System.SysUtils,
  System.TypInfo,
  Daf.MiniSpec,
  Daf.SQLCute,
  Daf.SQLCute.Clauses;

type
  TOperatorWorld = class
  public
    /// El texto tal cual lo escribiria quien llama. Los nombres de estos campos son los que
    /// la tabla de ejemplos inyecta por RTTI: cambiarlos rompe la sustitucion.
    Op: string;
    Expected: string;
    Actual: string;
    /// Clase y mensaje del fallo, o un centinela con texto si no lo hubo. Afirmar solo que
    /// "fallo" daria por bueno cualquier fallo, incluido uno que no viene al caso.
    Outcome: string;
  end;

function OpName(const Value: TWhereOp): string;
begin
  Result := GetEnumName(System.TypeInfo(TWhereOp), Ord(Value));
end;

initialization

Feature('''
Feature Un operador escrito como texto se lee siempre igual @sqlcute @where @operators

  Como quien escribe una condicion con Where(col, op, valor)
  Quiero que cada operador que la documentacion admite se traduzca al mismo operador del arbol
  Para que cambiar como se resuelve la tabla no cambie en silencio lo que emite una consulta
''')

.UseWorld<TOperatorWorld>

.Rule('Cada operador admitido se lee como el suyo')

  .ScenarioOutline('<Op> se lee como <Expected>')
    .Given('el operador escrito <Op>')
    .When('se interpreta', procedure(W: TOperatorWorld)
      begin
        W.Actual := OpName(ParseWhereOp(W.Op));
      end)
    .&Then('da el operador <Expected>', procedure(W: TOperatorWorld)
      begin
        Expect(W.Actual).ToEqual(W.Expected);
      end)
    .Examples(
      [['Op', 'Expected'],
       ['=', 'Equal'],
       ['<>', 'NotEqual'],
       ['!=', 'NotEqual'],
       ['<', 'Less'],
       ['<=', 'LessOrEqual'],
       ['>', 'Greater'],
       ['>=', 'GreaterOrEqual'],
       ['like', 'Like'],
       ['not like', 'NotLike'],
       ['in', 'In'],
       ['not in', 'NotIn']])

.Rule('La lectura no depende de como se escriba alrededor')

  .ScenarioOutline('<Op> se lee igual escrito asi')
    .Given('el mismo operador escrito <Op>')
    .When('se interpreta', procedure(W: TOperatorWorld)
      begin
        W.Actual := OpName(ParseWhereOp(W.Op));
      end)
    .&Then('sigue dando <Expected>', procedure(W: TOperatorWorld)
      begin
        Expect(W.Actual).ToEqual(W.Expected);
      end)
    .Examples(
      // Mayusculas y espacios alrededor. No son entradas de la tabla: son las dos cosas que la
      // funcion hace ADEMAS de buscar, y las dos estaban sin afirmar.
      [['Op', 'Expected'],
       ['LIKE', 'Like'],
       ['Like', 'Like'],
       ['NOT LIKE', 'NotLike'],
       ['IN', 'In'],
       ['NOT IN', 'NotIn'],
       [' = ', 'Equal'],
       ['  not in  ', 'NotIn']])

.Rule('Lo que no esta en la tabla se dice, no se adivina')

  .Scenario('UnOperadorDesconocidoAbortaNombrandolo')
    .Given('un operador que la tabla no contiene', procedure(W: TOperatorWorld)
      begin
        W.Op := '=~';
        W.Outcome := '<sin intentar>';
      end)
    .When('se interpreta', procedure(W: TOperatorWorld)
      begin
        W.Outcome := '<sin fallo>';
        try
          W.Actual := OpName(ParseWhereOp(W.Op));
        except
          on E: Exception do W.Outcome := E.ClassName + ': ' + E.Message;
        end;
      end)
    .&Then('aborta nombrando el operador recibido', procedure(W: TOperatorWorld)
      begin
        // Sobre el mensaje y no sobre que lanzara: un fallo cualquiera pasaria lo segundo, y
        // quien escribio la cadena necesita ver CUAL no se reconocio.
        Expect(W.Outcome).ToContain('=~');
      end)
    .&And('y es el fallo de argumento, no otro', procedure(W: TOperatorWorld)
      begin
        Expect(W.Outcome).ToContain('EArgumentException');
      end)

  .Scenario('UnaCadenaVaciaTambienAborta')
    .Given('un operador vacio', procedure(W: TOperatorWorld)
      begin
        W.Op := '   ';
        W.Outcome := '<sin intentar>';
      end)
    .When('se interpreta', procedure(W: TOperatorWorld)
      begin
        W.Outcome := '<sin fallo>';
        try
          W.Actual := OpName(ParseWhereOp(W.Op));
        except
          on E: Exception do W.Outcome := E.ClassName + ': ' + E.Message;
        end;
      end)
    .&Then('aborta en vez de leerse como alguno', procedure(W: TOperatorWorld)
      begin
        Expect(W.Outcome).ToContain('EArgumentException');
      end);

end.
