unit Macros.Feat;

{
  Coverage for TMacrosList, the live consumer of TExpresion<string>
  (TMacroExpression). A macro genuinely is "a value or a function producing a
  value", which is what TExpresion<T> is for — so this is the type's legitimate
  use, and this spec is the net that keeps it working while the unused
  TBooleanExpresion is removed from the same unit.
}

interface

type
  TMacrosWorld = class
  public
    Constant: string;
    FromThunk: string;
    Resolved: string;
    Count: Integer;
    Has: Boolean;
  end;

implementation

uses
  System.SysUtils,
  Daf.MiniSpec,
  Daf.Expression,
  Daf.Types;

initialization

Feature('''
Feature Commons macro expressions @commons @macros

  As a developer composing configurable strings
  I want a macro to be either a constant or a function evaluated on demand
  So values that change at run time resolve at use time, not at registration time
''')

.UseWorld<TMacrosWorld>

.Rule('A macro holds either a constant or a thunk')

  .Scenario('A constant macro round-trips')
    .When('a macro is registered from a plain string',
      procedure(W: TMacrosWorld)
      begin
        var Macros := TMacrosList.Create;
        try
          Macros['name'] := 'DAF';
          W.Constant := Macros['name'];
          W.Count := Macros.Count;
          W.Has := Macros.Contains('name');
        finally
          Macros.Free;
        end;
      end)
    .&Then('the same value comes back out',
      procedure(W: TMacrosWorld)
      begin
        Expect(W.Constant).ToEqual('DAF');
        Expect(W.Count).ToEqual(1);
        Expect(W.Has).ToBeTrue;
      end)

  .Scenario('A thunk macro is evaluated when read, not when registered')
    .When('a macro is registered as a function whose source changes afterwards',
      procedure(W: TMacrosWorld)
      begin
        var Source := 'before';
        var Macros := TMacrosList.Create;
        try
          Macros['dyn'] := TFunc<string>(
            function: string
            begin
              Result := Source;
            end);
          Source := 'after';          // changed after registration
          W.FromThunk := Macros['dyn'];
        finally
          Macros.Free;
        end;
      end)
    .&Then('the value read reflects the state at read time',
      procedure(W: TMacrosWorld)
      begin
        Expect(W.FromThunk).ToEqual('after');
      end)

  .Scenario('Macros resolve inside a template')
    .When('a source string referencing two macros is resolved',
      procedure(W: TMacrosWorld)
      begin
        var Macros := TMacrosList.Create;
        try
          Macros['product'] := 'DAF';
          Macros['edition'] := TFunc<string>(
            function: string begin Result := 'Community'; end);
          W.Resolved := Macros.ResolveMacros('${product} ${edition}');
        finally
          Macros.Free;
        end;
      end)
    .&Then('both the constant and the thunk are substituted',
      procedure(W: TMacrosWorld)
      begin
        Expect(W.Resolved).ToContain('DAF');
        Expect(W.Resolved).ToContain('Community');
      end);

end.
