unit Arrays.Feat;

{
  Coverage for Commons' eager collection API (TSmartArray<T>): its filter and
  mapper vocabulary.

  The cross-API scenarios (the same filter or mapper value used with both the
  eager TSmartArray and the lazy Daf.Linq) live in LinqSpecs, which already
  depends on both units; Commons' own suite stays free of a Daf.Linq dependency.
}

interface

type
  TArraysWorld = class
  public
    Nums: TArray<Integer>;
    Mapped: TArray<string>;
    MappedByVar: TArray<string>;
    Filtered: TArray<Integer>;
    Composed: TArray<Integer>;
    AnyBig: Boolean;
    AnyHuge: Boolean;
  end;

implementation

uses
  System.SysUtils,
  Daf.MiniSpec,
  Daf.FilterSpec,
  Daf.Arrays;

initialization

Feature('''
Feature Commons eager collection @commons @arrays

  As a developer using Commons' eager TSmartArray<T>
  I want its filter and mapper operators to speak the framework's vocabulary
  So a filter or mapper value is written once and works with the lazy API too
''')

.UseWorld<TArraysWorld>

.Rule('Select projects every element')

  .Scenario('A bare closure projects each element')
    .When('Select maps each number to a label',
      procedure(W: TArraysWorld)
      begin
        var SA: TSmartArray<Integer> := TArray<Integer>.Create(5, 3, 8);
        W.Mapped := SA.Select<string>(
          function(X: Integer): string
          begin
            Result := 'n' + IntToStr(X);
          end);
      end)
    .&Then('the labels come out in source order',
      procedure(W: TArraysWorld)
      begin
        Expect(Length(W.Mapped)).ToEqual(3);
        Expect(W.Mapped[0]).ToEqual('n5');
        Expect(W.Mapped[1]).ToEqual('n3');
        Expect(W.Mapped[2]).ToEqual('n8');
      end)

  .Scenario('A mapper held in a TFunc variable projects the same way')
    .When('the mapper is built once as a TFunc and then passed to Select',
      procedure(W: TArraysWorld)
      begin
        var SA: TSmartArray<Integer> := TArray<Integer>.Create(5, 3, 8);
        var Mapper: TFunc<Integer, string> :=
          function(X: Integer): string
          begin
            Result := 'n' + IntToStr(X);
          end;
        W.MappedByVar := SA.Select<string>(Mapper);
      end)
    .&Then('a reusable mapper value yields the same projection as an inline one',
      procedure(W: TArraysWorld)
      begin
        Expect(Length(W.MappedByVar)).ToEqual(3);
        Expect(W.MappedByVar[0]).ToEqual('n5');
        Expect(W.MappedByVar[2]).ToEqual('n8');
      end)

.Rule('Where and Any evaluate a filter spec')

  .Scenario('Where keeps the matching elements')
    .When('Where(x > 3) is applied',
      procedure(W: TArraysWorld)
      begin
        var SA: TSmartArray<Integer> := TArray<Integer>.Create(5, 3, 8);
        W.Filtered := SA.Where(
          function(X: Integer): Boolean
          begin
            Result := X > 3;
          end);
      end)
    .&Then('only 5 and 8 remain',
      procedure(W: TArraysWorld)
      begin
        Expect(Length(W.Filtered)).ToEqual(2);
        Expect(W.Filtered[0]).ToEqual(5);
        Expect(W.Filtered[1]).ToEqual(8);
      end)

  .Scenario('A composed filter spec is honored')
    .When('two specs are combined with and',
      procedure(W: TArraysWorld)
      begin
        var SA: TSmartArray<Integer> := TArray<Integer>.Create(5, 3, 8);
        var Big: TFilterSpec<Integer> := TPredicate<Integer>(
          function(X: Integer): Boolean begin Result := X > 3; end);
        var Even: TFilterSpec<Integer> := TPredicate<Integer>(
          function(X: Integer): Boolean begin Result := X mod 2 = 0; end);
        W.Composed := SA.Where(Big and Even);
      end)
    .&Then('only 8 satisfies both',
      procedure(W: TArraysWorld)
      begin
        Expect(Length(W.Composed)).ToEqual(1);
        Expect(W.Composed[0]).ToEqual(8);
      end)

  .Scenario('Any reports whether some element matches')
    .When('Any is asked for an element over 7 and for one over 100',
      procedure(W: TArraysWorld)
      begin
        var SA: TSmartArray<Integer> := TArray<Integer>.Create(5, 3, 8);
        W.AnyBig := SA.Any(
          function(X: Integer): Boolean begin Result := X > 7; end);
        W.AnyHuge := SA.Any(
          function(X: Integer): Boolean begin Result := X > 100; end);
      end)
    .&Then('the first is True and the second False',
      procedure(W: TArraysWorld)
      begin
        Expect(W.AnyBig).ToBeTrue;
        Expect(W.AnyHuge).ToBeFalse;
      end);

end.
