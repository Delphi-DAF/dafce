unit Calculator.Engine;

interface

uses
  System.SysUtils;

type
  TCalculator = class
  strict private
    FResult: Integer;
  public
    procedure Add(A, B: Integer);
    procedure Subtract(A, B: Integer);
    procedure Mult(A, B: Integer);
    procedure Divide(A, B: Integer);
    property Result: Integer read FResult;
  end;

implementation

procedure TCalculator.Add(A, B: Integer);
begin
  FResult := A + B;
end;

procedure TCalculator.Subtract(A, B: Integer);
begin
  FResult := A - B;
end;

procedure TCalculator.Mult(A, B: Integer);
begin
  FResult := A * B;
end;

procedure TCalculator.Divide(A, B: Integer);
begin
  if B = 0 then
    raise EDivByZero.Create('Division by zero');
  FResult := A div B;
end;

end.
