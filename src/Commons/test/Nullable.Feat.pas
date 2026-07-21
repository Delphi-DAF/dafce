unit Nullable.Feat;

interface

uses
  Daf.Nullable;

type
  TNullableWorld = class
  public
    IntNull: TNullable<Integer>;
    IntZero: TNullable<Integer>;
    Materialized: TNullable<Integer>;
    RoundTrip: Integer;
  end;

implementation

uses
  Daf.MiniSpec;

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
    .&Then('the null one reports IsNull True and HasValue False',
      procedure(W: TNullableWorld)
      begin
        Expect(W.IntNull.IsNull).ToBeTrue;
        Expect(W.IntNull.HasValue).ToBeFalse;
      end)
    .&And('the zero one reports IsNull False, HasValue True and Value 0',
      procedure(W: TNullableWorld)
      begin
        Expect(W.IntZero.IsNull).ToBeFalse;
        Expect(W.IntZero.HasValue).ToBeTrue;
        Expect(W.IntZero.Value).ToEqual(0);
      end)

  .Scenario('A freshly declared nullable is null')
    .Given('a default-initialized nullable').NoAction
    .&Then('it reports IsNull True',
      procedure(W: TNullableWorld)
      begin
        var N: TNullable<Integer>;
        Expect(N.IsNull).ToBeTrue;
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
    .&Then('GetValueOrDefault returns the supplied fallback',
      procedure(W: TNullableWorld)
      begin
        Expect(W.IntNull.GetValueOrDefault(99)).ToEqual(99);
      end);

end.
