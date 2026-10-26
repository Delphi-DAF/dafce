unit Mastermind.Game;

interface

type
  TMasterMindCode = TArray<Integer>;
  TMastermind = class
  private
    FSecret: TMasterMindCode;
    FGuess: TMasterMindCode;
    FCorrects: Byte;
    FMisplaceds: Byte;
    procedure SetGuess(const Value: TMasterMindCode);
    function GetGameOver: Boolean;
    procedure UpdateCounters;
  public
    constructor Create(const Secret: TMasterMindCode);
    property Guess: TMasterMindCode read FGuess write SetGuess;
    property Corrects: Byte read FCorrects;
    property Misplaceds: Byte read FMisplaceds;
    property GameOver: Boolean read GetGameOver;
  end;

implementation

constructor TMastermind.Create(const Secret: TMasterMindCode);
begin
  inherited Create;
  FSecret := Secret;
end;

function TMastermind.GetGameOver: Boolean;
begin
  Result := FCorrects = 4;
end;

procedure TMastermind.UpdateCounters;
begin
  FCorrects := 0;
  FMisplaceds := 0;
  for var GuessIdx := 0 to 3 do
  begin
    for var SecretIdx := 0 to 3 do
    begin
      if (FGuess[GuessIdx] = FSecret[SecretIdx]) then
      begin
        if GuessIdx = SecretIdx then
          Inc(FCorrects)
        else
          Inc(FMisplaceds);
      end;
    end;
  end;
end;

procedure TMastermind.SetGuess(const Value: TMasterMindCode);
begin
  FGuess := Value;
  UpdateCounters;
end;

end.
