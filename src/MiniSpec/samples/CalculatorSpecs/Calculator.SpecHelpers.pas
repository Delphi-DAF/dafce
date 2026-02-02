unit Calculator.SpecHelpers;

interface

uses
  System.SysUtils,
  Calculator.Engine;

type
  TCalculatorWorld = class
  public
    A, B, Result: Integer;
    Calculator: TCalculator;
    constructor Create;
    destructor Destroy;override;
  end;

implementation

{ TCalculatorWorld }

constructor TCalculatorWorld.Create;
begin
  inherited;
end;

destructor TCalculatorWorld.Destroy;
begin
  FreeAndNil(Calculator);
  inherited;
end;

end.
