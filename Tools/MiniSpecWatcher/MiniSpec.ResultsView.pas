unit MiniSpec.ResultsView;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Threading,
  System.Win.TaskbarCore,
  System.Actions,
  System.ImageList,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Taskbar,
  Vcl.ImgList,
  Vcl.ActnList,
  Vcl.ComCtrls,
  Vcl.StdActns,
  Vcl.AppEvnts,
  Vcl.Menus,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  MiniSpec.WatcherEngine, Vcl.TitleBarCtrls, Vcl.Samples.Spin, Vcl.Imaging.pngimage;

type
  TUseMode = (umNotification, umDesktop);
  TResultsView = class(TForm)
    ActionList: TActionList;
    SpecsRunAct: TAction;
    SpecsViewReportAct: TAction;
    ImageList: TImageList;
    PassCounterCtl: TLabel;
    FailCounterCtl: TLabel;
    ReportLayout: TGridPanel;
    PassValueCtl: TLabel;
    FailsValueCtl: TLabel;
    TrayIcon: TTrayIcon;
    OpenReportBtm: TButton;
    RunSpecsBtn: TButton;
    TitleCtl: TLabel;
    ReportHEaderCtl: TPanel;
    HideBtn: TButton;
    ExitBtn: TButton;
    PageControl: TPageControl;
    ReportArea: TTabSheet;
    BottomBar: TGridPanel;
    StatusBar: TStatusBar;
    WindowExitAct: TAction;
    Timer: TTimer;
    AboutArea: TTabSheet;
    Label2: TLabel;
    Label3: TLabel;
    mSpecLogo: TImage;
    procedure SpecsRunActExecute(Sender: TObject);
    procedure SpecsViewReportActExecute(Sender: TObject);
    procedure TrayIconClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure WindowCloseActExecute(Sender: TObject);
    procedure SpecsViewReportActUpdate(Sender: TObject);
    procedure SpecsRunActUpdate(Sender: TObject);
    procedure WindowExitActExecute(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure StatusBarMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    FWatcherEngine: TWatcherEngine;
    FUseMode: TUseMode;
    FReportInfo: TReportInfo;
    procedure ShowInBottomRight;
    procedure ShowCentered;
    procedure OnReportReady(Sender: TWatcherEngine; const NewReportInfo: TReportInfo);
    procedure NotifyReport;
    procedure BeginDesktopMode;
    procedure EndDesktopMode;
    procedure SetReportInfo(const Value: TReportInfo);
    procedure SetWatcherEngine(const Value: TWatcherEngine);
  public
    constructor Create(Owner: TComponent);override;
    destructor Destroy;override;
    property WatcherEngine: TWatcherEngine read FWatcherEngine write SetWatcherEngine;
    property UseMode: TUseMode read FUseMode;
    property ReportInfo: TReportInfo read FReportInfo write SetReportInfo;
  end;

var
  ResultsView: TResultsView;

implementation
uses
  WinApi.Windows,
  WinApi.Messages,
  System.IOUtils;

{$R *.dfm}

{ TResultsView }

constructor TResultsView.Create(Owner: TComponent);
begin
  inherited;
  Position := poDesigned;
  FUseMode := umNotification;
  PassValueCtl.Caption := '';
  FailsValueCtl.Caption := '';
  TitleCtl.Caption := 'MiniSpec Watcher Ready!';
  EndDesktopMode;
end;

destructor TResultsView.Destroy;
begin
  FWatcherEngine.Stop;
  FWatcherEngine.OnReportReady := nil;
  FWatcherEngine.Free;
  inherited;
end;

procedure TResultsView.SpecsRunActExecute(Sender: TObject);
begin
  FWatcherEngine.RunSpecs(ReportInfo.ExeFile);
end;

procedure TResultsView.SpecsRunActUpdate(Sender: TObject);
begin
  SpecsRunAct.Enabled := not FReportInfo.ExeFile.IsEmpty
end;

procedure TResultsView.SpecsViewReportActExecute(Sender: TObject);
begin
  FWatcherEngine.ViewReport(FReportInfo.LinkToReport);
end;

procedure TResultsView.SpecsViewReportActUpdate(Sender: TObject);
begin
  SpecsViewReportAct.Enabled := not ReportInfo.LinkToReport.IsEmpty
end;

procedure TResultsView.StatusBarMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    ReleaseCapture;
    SendMessage(Handle, WM_NCLBUTTONDOWN, HTCAPTION, 0);
  end;
end;

procedure TResultsView.FormShow(Sender: TObject);
begin
  if UseMode = umNotification then
    ShowInBottomRight
  else
    ShowCentered;
end;

procedure TResultsView.ShowInBottomRight;
var
  WorkArea: TRect;
begin
  SystemParametersInfo(SPI_GETWORKAREA, 0, @WorkArea, 0); // Área usable (sin taskbar)
  Left := WorkArea.Right - Width;
  Top := WorkArea.Bottom - Height;
end;

procedure TResultsView.ShowCentered;
var
  WorkArea: TRect;
begin
  SystemParametersInfo(SPI_GETWORKAREA, 0, @WorkArea, 0); // Área usable (sin taskbar)
  Left := WorkArea.Left + ((WorkArea.Right - WorkArea.Left) - Width) div 2;
  Top  := WorkArea.Top + ((WorkArea.Bottom - WorkArea.Top) - Height) div 2;
end;

procedure TResultsView.WindowExitActExecute(Sender: TObject);
begin
  Hide;
  FWatcherEngine.Stop;
  Close;
end;

procedure TResultsView.BeginDesktopMode;
begin
  FUseMode := umDesktop;
  PageControl.ActivePage := ReportArea;
  AlphablendValue := 255;
  FormStyle := TFormStyle.fsStayOnTop;
  Show;
  TrayIcon.Visible := True;
end;

procedure TResultsView.EndDesktopMode;
begin
  AlphablendValue := 0;
  Hide;
  FUseMode := umNotification;
  PageControl.ActivePage := ReportArea;
  FormStyle := TFormStyle.fsStayOnTop;
  TrayIcon.Visible := True;
end;

procedure TResultsView.TimerTimer(Sender: TObject);
begin
  //don't delete:  force windows messages proccessing
end;

procedure TResultsView.TrayIconClick(Sender: TObject);
begin
  BeginDesktopMode;
end;

procedure TResultsView.WindowCloseActExecute(Sender: TObject);
begin
  EndDesktopMode;
end;

procedure TResultsView.NotifyReport;
begin
  if ReportInfo.IsEmpty then
    StatusBar.Panels[0].Text := 'report fail'
  else
    StatusBar.Panels[0].Text := 'New report';
  if UseMode = umDesktop then
  begin
    Log('UsMode = umDesktop' );
    StatusBar.Update;
    Sleep(500);
    StatusBar.Panels[0].Text := '';
    Application.BringToFront;
    Exit;
  end;
  Log('UsMode = umNotification' );
  AlphablendValue := 255;
  PageControl.ActivePage := ReportArea;
  Show;
  Update;
  Sleep(2000);
  for var idx := 50 downto 0 do
  begin
    AlphablendValue := 5*idx;
    Update;
    Sleep(1);
  end;
  AlphablendValue := 0;
  Hide;
end;

procedure TResultsView.SetReportInfo(const Value: TReportInfo);
begin
  FReportInfo := Value;
  PassValueCtl.Caption := ReportInfo.PassCount.ToString;
  FailsValueCtl.Caption := ReportInfo.FailsCount.ToString;
  TitleCtl.Caption := ExtractFileName(ReportInfo.ExeFile);
end;

procedure TResultsView.SetWatcherEngine(const Value: TWatcherEngine);
begin
  FWatcherEngine := Value;
  FWatcherEngine.OnReportReady := OnReportReady;
  StatusBar.Panels[1].Text := 'watching: ' + FWatcherEngine.WatchDir;
  NotifyReport;
  FWatcherEngine.Start;
end;

procedure TResultsView.OnReportReady(Sender: TWatcherEngine; const NewReportInfo: TReportInfo);
begin
  TrayIcon.BalloonTitle := FormatDateTime('yyyy-mm-dd', Now);
  TThread.Queue(nil, procedure
  begin
    Log('OnReportReady');
    ReportInfo := NewReportInfo;
    NotifyReport;
  end);
end;

end.
