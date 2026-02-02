unit ConsoleSample.AppSettings;

interface

type
  {$M+}
  TAppSettings = class
  private
    FVersion: string;
    FMessage: string;
    FTitle: string;
    procedure SetMessage(const Value: string);
    procedure SetTitle(const Value: string);
    procedure SetVersion(const Value: string);
  public
    property Title: string read FTitle write SetTitle;
    property Version: string read FVersion write SetVersion;
    property Message: string read FMessage write SetMessage;
  end;
  {$M-}

implementation

{ TAppSettings }

procedure TAppSettings.SetMessage(const Value: string);
begin
  FMessage := Value;
end;

procedure TAppSettings.SetTitle(const Value: string);
begin
  FTitle := Value;
end;

procedure TAppSettings.SetVersion(const Value: string);
begin
  FVersion := Value;
end;

end.
