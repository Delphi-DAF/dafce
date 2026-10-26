unit PropertyAssign.Feat;

{
  Lo que TRttiProperty.SetValue de Daf.Rtti acepta en una propiedad de una clase. El valor llega
  con el tipo de su origen, no con el de la propiedad: un Variant leido de una columna tinyint es
  un ShortInt, y un Integer 0 es lo que queda cuando un NULL se sustituye por su valor por defecto.
  Lo que SetPropValue convierte, el helper lo convierte; lo que SetPropValue rechaza, lo rechaza.
}

interface

type
  TAssignState = (Idle, Busy, Down);
  TAssignColor = (Red, Green, Blue);
  TAssignColors = set of TAssignColor;

  {$M+}
  TAssignTarget = class
  private
    FFlag: Boolean;
    FPublicFlag: Boolean;
    FState: TAssignState;
    FCount: Integer;
    FBig: Int64;
    FTitle: string;
    FLetter: Char;
    FColors: TAssignColors;
  public
    property PublicFlag: Boolean read FPublicFlag write FPublicFlag;
  published
    property Flag: Boolean read FFlag write FFlag;
    property State: TAssignState read FState write FState;
    property Count: Integer read FCount write FCount;
    property Big: Int64 read FBig write FBig;
    property Title: string read FTitle write FTitle;
    property Letter: Char read FLetter write FLetter;
    property Colors: TAssignColors read FColors write FColors;
  end;
  {$M-}

  TPropertyAssignWorld = class
  public
    Prop: string;
    Kind: string;
    Raw: string;
    Expected: string;
    Outcome: string;
    Target: TAssignTarget;
    constructor Create;
    destructor Destroy; override;
    procedure Assign;
    /// Coerce un TValue vacio hacia TNullable<Integer> y deja en Outcome si pudo y si quedo ausente.
    procedure CoerceEmptyToNullable;
  end;

implementation

uses
  System.SysUtils,
  System.Rtti,
  Daf.MiniSpec,
  Daf.Nullable,
  Daf.Rtti;

function SourceVariant(const Kind, Raw: string): Variant;
begin
  if Kind = 'ShortInt' then
    Result := ShortInt(StrToInt(Raw))
  else if Kind = 'Integer' then
    Result := StrToInt(Raw)
  else if Kind = 'string' then
    Result := Raw
  else
    raise Exception.CreateFmt('Kind desconocido en los ejemplos: %s', [Kind]);
end;

{ TPropertyAssignWorld }

constructor TPropertyAssignWorld.Create;
begin
  inherited;
  Target := TAssignTarget.Create;
end;

destructor TPropertyAssignWorld.Destroy;
begin
  Target.Free;
  inherited;
end;

procedure TPropertyAssignWorld.Assign;
begin
  var Ctx := TRttiContext.Create;
  var RP := Ctx.GetType(TAssignTarget).GetProperty(Prop);
  // Un nombre mal escrito en los ejemplos tiene que salir como tal, no como un valor vacio.
  if RP = nil then
  begin
    Outcome := 'no property ' + Prop;
    Exit;
  end;
  try
    RP.SetValue(Target, TValue.FromVariant(SourceVariant(Kind, Raw)));
    Outcome := RP.GetValue(Target).ToString;
  except
    on E: Exception do Outcome := 'raises ' + E.ClassName;
  end;
end;

procedure TPropertyAssignWorld.CoerceEmptyToNullable;
begin
  var Ctx := TRttiContext.Create;
  var Res: TValue;
  var Coerced := CoerceForAssign(Ctx.GetType(TypeInfo(TNullable<Integer>)), TValue.Empty, Res);
  var Absent := Coerced and Res.AsType<TNullable<Integer>>.IsNull;
  Outcome := Format('coerced=%s, absent=%s', [BoolToStr(Coerced, True), BoolToStr(Absent, True)]);
end;

initialization

Feature('''
Feature Assigning a value to a property through RTTI @commons @rtti @assign

  As a framework author filling an object from values read elsewhere
  I want a value converted to the type of the property it is assigned to
  So that a column read as one type fills a property declared as another
''')

.UseWorld<TPropertyAssignWorld>

.Rule('A value is converted to the property the way SetPropValue converts it')

  .ScenarioOutline('<Kind> <Raw> assigned to <Prop> stores <Expected>')
    .When('the <Kind> <Raw> is assigned to <Prop>',
      procedure(W: TPropertyAssignWorld)
      begin
        W.Assign;
      end)
    .&Then('<Prop> holds <Expected>',
      procedure(W: TPropertyAssignWorld)
      begin
        Expect(W.Outcome).ToEqual(W.Expected);
      end)
    .Examples(
      [['Prop',       'Kind',     'Raw',     'Expected'],
       ['Flag',       'ShortInt', '1',       'True'],
       ['Flag',       'Integer',  '0',       'False'],
       ['PublicFlag', 'ShortInt', '1',       'True'],
       ['State',      'Integer',  '2',       'Down'],
       ['State',      'string',   'Busy',    'Busy'],
       ['Count',      'string',   '7',       '7'],
       ['Big',        'string',   '7',       '7'],
       ['Title',      'Integer',  '42',      '42'],
       ['Letter',     'Integer',  '65',      'A'],
       ['Colors',     'string',   '[Green]', '[Green]']])

.Rule('A value the property cannot hold is rejected, not bent into it')

  .ScenarioOutline('<Kind> <Raw> assigned to <Prop> raises <Expected>')
    // Un 2 en un Boolean no es True: el rango de la enumeracion es 0..1 y se respeta.
    .When('the <Kind> <Raw> is assigned to <Prop>',
      procedure(W: TPropertyAssignWorld)
      begin
        W.Assign;
      end)
    .&Then('the assignment raises <Expected>',
      procedure(W: TPropertyAssignWorld)
      begin
        Expect(W.Outcome).ToEqual('raises ' + W.Expected);
      end)
    .Examples(
      [['Prop',  'Kind',     'Raw', 'Expected'],
       ['Flag',  'ShortInt', '2',   'ERangeError'],
       ['State', 'Integer',  '3',   'ERangeError'],
       ['Count', 'string',   'x',   'EVariantTypeCastError']])

.Rule('A NULL value leaves a TNullable absent')

  .Scenario('An empty TValue towards TNullable')
    .When('an empty TValue is coerced towards TNullable<Integer>',
      procedure(W: TPropertyAssignWorld)
      begin
        W.CoerceEmptyToNullable;
      end)
    .&Then('the result is absent',
      procedure(W: TPropertyAssignWorld)
      begin
        Expect(W.Outcome).ToEqual('coerced=True, absent=True');
      end);

end.
