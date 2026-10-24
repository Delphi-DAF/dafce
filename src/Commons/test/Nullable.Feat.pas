unit Nullable.Feat;

interface

uses
  System.SysUtils,
  Daf.Nullable;

type
  TNullableWorld = class
  public
    IntNull: TNullable<Integer>;
    IntZero: TNullable<Integer>;
    Materialized: TNullable<Integer>;
    RoundTrip: Integer;
    Report: string;
    Fallback: Integer;
    /// Ejecuta una lectura y deja en Report su resultado o la excepcion que lanzo.
    procedure Attempt(const Read: TFunc<Integer>);
  end;

implementation

uses
  Daf.MiniSpec;

procedure TNullableWorld.Attempt(const Read: TFunc<Integer>);
begin
  try
    Report := 'read ' + IntToStr(Read());
  except
    on E: Exception do Report := E.ClassName + ': ' + E.Message;
  end;
end;

initialization

Feature('''
Feature Nullable @nullable @commons

  As a developer
  I want an Optional/Maybe value type
  So I can distinguish an absent value (SQL NULL) from a present default
''')

.UseWorld<TNullableWorld>

.Rule('Presence predicates')

  .Scenario('Null is distinct from zero')
    .Given('a null nullable and one holding 0',
      procedure(W: TNullableWorld)
      begin
        W.IntNull := TNullable<Integer>.Null;
        W.IntZero := 0;
      end)
    .When('each one is asked about its presence',
      procedure(W: TNullableWorld)
      begin
        W.Report := Format('null: IsNull=%s HasValue=%s | zero: IsNull=%s HasValue=%s Value=%d',
          [BoolToStr(W.IntNull.IsNull, True), BoolToStr(W.IntNull.HasValue, True),
           BoolToStr(W.IntZero.IsNull, True), BoolToStr(W.IntZero.HasValue, True),
           W.IntZero.Value]);
      end)
    .&Then('the null one reports IsNull True and HasValue False, and the zero one the opposite',
      procedure(W: TNullableWorld)
      begin
        Expect(W.Report).ToEqual(
          'null: IsNull=True HasValue=False | zero: IsNull=False HasValue=True Value=0');
      end)

  .Scenario('A freshly declared nullable is null')
    // Nothing to prepare: the declaration IS the subject. It stays inside the When and not in
    // the World, because a field is zero-filled when the object is created, so it would report
    // IsNull True even with no Initialize operator at all.
    .Given('nothing declared yet').NoAction
    .When('a nullable is declared and asked about its presence',
      procedure(W: TNullableWorld)
      begin
        var N: TNullable<Integer>;
        W.Report := BoolToStr(N.IsNull, True);
      end)
    .&Then('it reports IsNull True',
      procedure(W: TNullableWorld)
      begin
        Expect(W.Report).ToEqual('True');
      end)

.Rule('Implicit conversions')

  .Scenario('Assigning a value makes it present')
    .When('a value is implicitly assigned',
      procedure(W: TNullableWorld)
      begin
        W.Materialized := 42;
      end)
    .&Then('HasValue is True and Value is the assigned value',
      procedure(W: TNullableWorld)
      begin
        Expect(W.Materialized.HasValue).ToBeTrue;
        Expect(W.Materialized.Value).ToEqual(42);
      end)

  .Scenario('A present nullable converts back to its value')
    .Given('a nullable holding 7',
      procedure(W: TNullableWorld)
      begin
        W.Materialized := 7;
      end)
    .When('it is implicitly converted to Integer',
      procedure(W: TNullableWorld)
      begin
        W.RoundTrip := W.Materialized;
      end)
    .&Then('the value round-trips',
      procedure(W: TNullableWorld)
      begin
        Expect(W.RoundTrip).ToEqual(7);
      end)

  .Scenario('GetValueOrDefault returns the fallback when null')
    .Given('a null nullable',
      procedure(W: TNullableWorld)
      begin
        W.IntNull := TNullable<Integer>.Null;
      end)
    .When('GetValueOrDefault is called with a fallback',
      procedure(W: TNullableWorld)
      begin
        W.Fallback := W.IntNull.GetValueOrDefault(99);
      end)
    .&Then('it returns the supplied fallback',
      procedure(W: TNullableWorld)
      begin
        Expect(W.Fallback).ToEqual(99);
      end)

.Rule('Reading the value of an absent nullable raises')

  .Scenario('An absent nullable does not convert')
    .Given('an absent nullable',
      procedure(W: TNullableWorld)
      begin
        W.IntNull := TNullable<Integer>.Null;
      end)
    .When('it is assigned to an Integer variable',
      procedure(W: TNullableWorld)
      begin
        W.Attempt(
          function: Integer
          begin
            W.RoundTrip := W.IntNull;
            Result := W.RoundTrip;
          end);
      end)
    .&Then('the assignment raises EInvalidOpException',
      procedure(W: TNullableWorld)
      begin
        Expect(W.Report).ToEqual('EInvalidOpException: Nullable object must have a value.');
      end)

  .Scenario('Value of an absent nullable raises')
    .Given('an absent nullable',
      procedure(W: TNullableWorld)
      begin
        W.IntNull := TNullable<Integer>.Null;
      end)
    .When('its Value is read',
      procedure(W: TNullableWorld)
      begin
        W.Attempt(
          function: Integer
          begin
            Result := W.IntNull.Value;
          end);
      end)
    .&Then('the read raises EInvalidOpException',
      procedure(W: TNullableWorld)
      begin
        Expect(W.Report).ToEqual('EInvalidOpException: Nullable object must have a value.');
      end)

  .Scenario('A present nullable converts to its value')
    .Given('a nullable holding 7',
      procedure(W: TNullableWorld)
      begin
        W.Materialized := 7;
      end)
    .When('it is assigned to an Integer variable and its Value is read',
      procedure(W: TNullableWorld)
      begin
        W.RoundTrip := W.Materialized;
        W.Attempt(
          function: Integer
          begin
            Result := W.Materialized.Value;
          end);
        W.Report := Format('assigned %d, %s', [W.RoundTrip, W.Report]);
      end)
    .&Then('both reads give 7',
      procedure(W: TNullableWorld)
      begin
        Expect(W.Report).ToEqual('assigned 7, read 7');
      end);

end.
