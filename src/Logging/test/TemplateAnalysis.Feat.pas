unit TemplateAnalysis.Feat;

{
  Aplazar el analisis de la plantilla no cambia lo que se obtiene. Estos specs tienen que estar
  VERDES antes y despues del refactor: su trabajo es cazar el cambio, no describir algo nuevo. El
  rojo que demuestra que muerden se ve rompiendo a mano la memoizacion --que la segunda lectura
  devuelva vacio-- y viendo caer el escenario de las dos lecturas.
}

interface

uses
  System.SysUtils,
  System.Rtti,
  System.Generics.Collections,
  Daf.Extensions.Logging;

type
  TTemplateWorld = class
  public
    First: string;
    Second: string;
    MsgRead: string;
    MsgUnread: string;
    Names: string;
  end;

/// Serializa los pares de una entrada como "clave=valor;", en orden, para poder compararlos.
function DumpPairs(const Pairs: TArray<TPair<string, TValue>>): string;
function V(const S: string): TValue;

implementation

uses
  Daf.MiniSpec;

function V(const S: string): TValue;
begin
  Result := TValue.From<string>(S);
end;

function DumpPairs(const Pairs: TArray<TPair<string, TValue>>): string;
begin
  Result := '';
  for var Pair in Pairs do
    Result := Result + Pair.Key + '=' + Pair.Value.ToString + ';';
end;

initialization

Feature('''
Feature Deferring the template analysis does not change what you get @logging @template

  As someone who moved the template parse off the hot path
  I want reading the pairs once, twice or never to give the same result
  So that the saving costs nothing in correctness
''')

.UseWorld<TTemplateWorld>

.Rule('Reading it twice is the same as reading it once')

  .Scenario('Two reads give the same names and values')
    .When('the pairs of an entry are read twice in a row',
      procedure(W: TTemplateWorld)
      begin
        var State := TLogState.Create('{User} did {Action}', [V('alice'), V('login')]);
        W.First := DumpPairs(State.KeyValuePairs);
        W.Second := DumpPairs(State.KeyValuePairs);
      end)
    .&Then('the first read has the names paired with their values',
      procedure(W: TTemplateWorld)
      begin
        Expect(W.First).ToEqual('User=alice;Action=login;');
      end)
    .&And('and the second read is identical -- this is the one a broken memoization fails',
      procedure(W: TTemplateWorld)
      begin
        Expect(W.Second).ToEqual(W.First);
      end)

  .Scenario('Never reading the pairs does not change the message')
    .When('one entry is formatted after reading its pairs and an identical one without',
      procedure(W: TTemplateWorld)
      begin
        var Reads := TLogState.Create('paid {Amount} to {Who}', [V('10'), V('Acme')]);
        var Ignore := DumpPairs(Reads.KeyValuePairs);
        W.MsgRead := Reads.Formatter(Reads, nil);

        var Unread := TLogState.Create('paid {Amount} to {Who}', [V('10'), V('Acme')]);
        W.MsgUnread := Unread.Formatter(Unread, nil);
      end)
    .&Then('both messages came out formatted the same',
      procedure(W: TTemplateWorld)
      begin
        Expect(W.MsgUnread).ToEqual(W.MsgRead);
      end)
    .&And('and it is the message the template describes',
      procedure(W: TTemplateWorld)
      begin
        Expect(W.MsgRead).ToEqual('paid 10 to Acme');
      end)

.Rule('What counts as a template name does not change')

  .Scenario('A plain name and an underscored name are both names')
    .When('a template with {User} and {User_2} is analysed',
      procedure(W: TTemplateWorld)
      begin
        var State := TLogState.Create('{User} {User_2}', [V('a'), V('b')]);
        W.Names := DumpPairs(State.KeyValuePairs);
      end)
    .&Then('both are declared, in order',
      procedure(W: TTemplateWorld)
      begin
        Expect(W.Names).ToEqual('User=a;User_2=b;');
      end)

  .Scenario('A non-ASCII name is not a name, because word-class without UCP is ASCII')
    .When('a template whose only placeholder holds an n-tilde is analysed',
      procedure(W: TTemplateWorld)
      begin
        var State := TLogState.Create('hola {año}', [V('x')]);
        W.Names := DumpPairs(State.KeyValuePairs);
      end)
    .&Then('it declares no name, exactly as before this change',
      procedure(W: TTemplateWorld)
      begin
        Expect(W.Names).ToEqual('');
      end);

end.
