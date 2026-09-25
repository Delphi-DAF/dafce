unit MissingMacros.Feat;

interface

uses
  Daf.Types;

type
  TMissingMacrosWorld = class
  public
    Macros: TMacrosList;
    Resolved: string;
    /// 'none', or the class and message of what the When raised. An unassigned string is
    /// already empty, so comparing against '' would pass even if the When never ran.
    Raised: string;
    /// The names the hook was asked for, in order.
    Asked: string;
    constructor Create;
    destructor Destroy; override;
    procedure Resolve(const Source: string);
    procedure Read(const Name: string);
    procedure ResolveElsewhere(Sender: TObject; const Name: string; var Value: string; var Handled: Boolean);
    procedure Decline(Sender: TObject; const Name: string; var Value: string; var Handled: Boolean);
  end;

implementation

uses
  System.SysUtils,
  Daf.MiniSpec;

{ TMissingMacrosWorld }

constructor TMissingMacrosWorld.Create;
begin
  inherited;
  Macros := TMacrosList.Create;
  Macros['root'] := '/srv/data';
end;

destructor TMissingMacrosWorld.Destroy;
begin
  Macros.Free;
  inherited;
end;

procedure TMissingMacrosWorld.Resolve(const Source: string);
begin
  try
    Resolved := Macros.ResolveMacros(Source);
    Raised := 'none';
  except
    on E: Exception do
      Raised := E.ClassName + ': ' + E.Message;
  end;
end;

procedure TMissingMacrosWorld.Read(const Name: string);
begin
  try
    var Value: string := Macros[Name];
    Resolved := '[' + Value + ']';
    Raised := 'none';
  except
    on E: Exception do
      Raised := E.ClassName + ': ' + E.Message;
  end;
end;

procedure TMissingMacrosWorld.ResolveElsewhere(Sender: TObject; const Name: string; var Value: string;
  var Handled: Boolean);
begin
  if Asked <> '' then
    Asked := Asked + ',';
  Asked := Asked + Name;
  Value := 'elsewhere';
  Handled := True;
end;

procedure TMissingMacrosWorld.Decline(Sender: TObject; const Name: string; var Value: string;
  var Handled: Boolean);
begin
  if Asked <> '' then
    Asked := Asked + ',';
  Asked := Asked + Name;
end;

initialization

Feature('''
Feature Commons macros that are not registered @commons @macros

  As a developer composing paths and messages from macros
  I want to decide what a reference to an unregistered macro becomes
  So a misspelt name can fail loudly instead of yielding a plausible string
''')

.UseWorld<TMissingMacrosWorld>

.Rule('Out of the box, a macro that is not registered resolves to empty')

  .Scenario('An unregistered macro resolves to empty when nothing is configured')
    .When('a string citing a macro that is not registered is resolved',
      procedure(W: TMissingMacrosWorld)
      begin
        W.Resolve('${missing}/charts');
      end)
    .&Then('the reference disappears and nothing is raised',
      procedure(W: TMissingMacrosWorld)
      begin
        Expect(W.Resolved).ToEqual('/charts');
        Expect(W.Raised).ToEqual('none');
      end)

.Rule('The policy decides what an unregistered macro becomes')

  .Scenario('mmpKeepToken leaves the reference in the result')
    .Given('the policy keeps the reference',
      procedure(W: TMissingMacrosWorld)
      begin
        W.Macros.MissingMacroPolicy := mmpKeepToken;
      end)
    .When('a string citing a registered macro and a missing one is resolved',
      procedure(W: TMissingMacrosWorld)
      begin
        W.Resolve('${root}/${missing}');
      end)
    .&Then('the registered one is substituted and the missing one stays readable',
      procedure(W: TMissingMacrosWorld)
      begin
        Expect(W.Resolved).ToEqual('/srv/data/${missing}');
      end)

  .Scenario('mmpReturnDefault substitutes MissingMacroValue')
    .Given('the policy substitutes a fallback value',
      procedure(W: TMissingMacrosWorld)
      begin
        W.Macros.MissingMacroPolicy := mmpReturnDefault;
        W.Macros.MissingMacroValue := 'unknown';
      end)
    .When('a string citing a macro that is not registered is resolved',
      procedure(W: TMissingMacrosWorld)
      begin
        W.Resolve('${missing}/charts');
      end)
    .&Then('the fallback takes the place of the reference',
      procedure(W: TMissingMacrosWorld)
      begin
        Expect(W.Resolved).ToEqual('unknown/charts');
      end)

  .Scenario('mmpError raises EMacroNotFound naming the macro')
    .Given('the policy raises',
      procedure(W: TMissingMacrosWorld)
      begin
        W.Macros.MissingMacroPolicy := mmpError;
      end)
    .When('a string citing a macro that is not registered is resolved',
      procedure(W: TMissingMacrosWorld)
      begin
        W.Resolve('${missing}/charts');
      end)
    .&Then('EMacroNotFound is raised and its message names the macro',
      procedure(W: TMissingMacrosWorld)
      begin
        Expect(W.Raised).ToContain('EMacroNotFound: ');
        Expect(W.Raised).ToContain('"missing"');
      end)

  .Scenario('Reading an unregistered macro by name does not apply the policy')
    .Given('the policy raises',
      procedure(W: TMissingMacrosWorld)
      begin
        W.Macros.MissingMacroPolicy := mmpError;
      end)
    .When('a macro that is not registered is read by name',
      procedure(W: TMissingMacrosWorld)
      begin
        W.Read('missing');
      end)
    .&Then('it reads as empty and nothing is raised',
      procedure(W: TMissingMacrosWorld)
      begin
        Expect(W.Resolved).ToEqual('[]');
        Expect(W.Raised).ToEqual('none');
      end)

.Rule('The hook runs before the policy')

  .Scenario('A hook that resolves wins over the policy')
    .Given('a hook that resolves, and the policy that raises',
      procedure(W: TMissingMacrosWorld)
      begin
        W.Macros.MissingMacroPolicy := mmpError;
        W.Macros.OnMissingMacro := W.ResolveElsewhere;
      end)
    .When('a string citing a registered macro and a missing one is resolved',
      procedure(W: TMissingMacrosWorld)
      begin
        W.Resolve('${root}/${missing}');
      end)
    .&Then('the hook supplies the value, and is asked only for the missing one',
      procedure(W: TMissingMacrosWorld)
      begin
        Expect(W.Raised).ToEqual('none');
        Expect(W.Resolved).ToEqual('/srv/data/elsewhere');
        Expect(W.Asked).ToEqual('missing');
      end)

  .Scenario('A hook that declines leaves the macro to the policy')
    .Given('a hook that declines, and a fallback value',
      procedure(W: TMissingMacrosWorld)
      begin
        W.Macros.MissingMacroValue := 'unknown';
        W.Macros.OnMissingMacro := W.Decline;
      end)
    .When('a string citing a macro that is not registered is resolved',
      procedure(W: TMissingMacrosWorld)
      begin
        W.Resolve('${missing}/charts');
      end)
    .&Then('the hook was asked, and the policy decided',
      procedure(W: TMissingMacrosWorld)
      begin
        Expect(W.Asked).ToEqual('missing');
        Expect(W.Resolved).ToEqual('unknown/charts');
      end)

  .Scenario('A hook can fail with the same exception as the policy')
    .Given('a hook that raises what MissingMacroError builds, and a policy that would not raise',
      procedure(W: TMissingMacrosWorld)
      begin
        W.Macros.MissingMacroPolicy := mmpReturnDefault;
        W.Macros.OnMissingMacro :=
          procedure(Sender: TObject; const Name: string; var Value: string; var Handled: Boolean)
          begin
            raise TMacrosList(Sender).MissingMacroError(Name);
          end;
      end)
    .When('a string citing a macro that is not registered is resolved',
      procedure(W: TMissingMacrosWorld)
      begin
        W.Resolve('${missing}/charts');
      end)
    .&Then('the class and the message are the ones the factory builds',
      procedure(W: TMissingMacrosWorld)
      begin
        var Expected := W.Macros.MissingMacroError('missing');
        try
          Expect(W.Raised).ToEqual(Expected.ClassName + ': ' + Expected.Message);
        finally
          Expected.Free;
        end;
      end)

.Rule('Resolution stops when a pass changes nothing')

  .Scenario('A reference left unresolved is not scanned again')
    .Given('the policy keeps the reference, and a hook that declines',
      procedure(W: TMissingMacrosWorld)
      begin
        W.Macros.MissingMacroPolicy := mmpKeepToken;
        W.Macros.OnMissingMacro := W.Decline;
      end)
    .When('a string citing only a macro that is not registered is resolved',
      procedure(W: TMissingMacrosWorld)
      begin
        W.Resolve('${missing}');
      end)
    .&Then('the hook is asked once, not once per recursion level',
      procedure(W: TMissingMacrosWorld)
      begin
        Expect(W.Asked).ToEqual('missing');
        Expect(W.Resolved).ToEqual('${missing}');
      end);

end.
