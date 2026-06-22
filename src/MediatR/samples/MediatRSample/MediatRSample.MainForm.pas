unit MediatRSample.MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.ComCtrls,
  Daf.MediatR.Contracts,
  MediatRSample.AppServices,
  MediatRSample.Requests,
  MediatRSample.Customer;

type
  TMainForm = class(TForm)
    TopArea: TPanel;
    NameLabel: TLabel;
    NameCtl: TEdit;
    AddCtl: TButton;
    RemoveCtl: TButton;
    FilterLabel: TLabel;
    FilterCtl: TEdit;
    SearchCtl: TButton;
    AllCtl: TButton;
    CustomersCtl: TListView;
    LogArea: TGroupBox;
    LogCtl: TMemo;
    procedure AddCtlClick(Sender: TObject);
    procedure RemoveCtlClick(Sender: TObject);
    procedure SearchCtlClick(Sender: TObject);
    procedure AllCtlClick(Sender: TObject);
  private
    FMediator: IMediator;
    FAppLog: IAppLog;
    procedure LoadCustomers(const FilterText: string = '');
  public
    procedure Initialize(const Mediator: IMediator; const AppLog: IAppLog);
    procedure Reload;
  end;

  // UI-refresh notification handlers.
  // Live in this unit to access the MainForm global; pure domain concerns go
  // in MediatRSample.Notifications instead.

  TFormCustomerAddedHandler = class(TNotificacionHandler<TCustomerAddedEvent>)
  public
    procedure Handle(Notification: TCustomerAddedEvent); override;
  end;

  TFormCustomerRemovedHandler = class(TNotificacionHandler<TCustomerRemovedEvent>)
  public
    procedure Handle(Notification: TCustomerRemovedEvent); override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

{ TMainForm }

procedure TMainForm.Initialize(const Mediator: IMediator; const AppLog: IAppLog);
begin
  FMediator := Mediator;
  FAppLog := AppLog;
  FAppLog.SetSink(procedure(Msg: string)
  begin
    LogCtl.Lines.Add(Msg);
    SendMessage(LogCtl.Handle, EM_SCROLL, SB_BOTTOM, 0);
  end);
  LoadCustomers;
end;

procedure TMainForm.LoadCustomers(const FilterText: string = '');
var
  Query: TCustomerQuery;
begin
  if FilterText.IsEmpty then
    Query := TCustomerQuery.Create
  else
    Query := TCustomerQuery.Create(
      function(C: TCustomer): Boolean
      begin
        Result := C.Name.ToLower.Contains(FilterText.ToLower);
      end);

  var Customers := FMediator.Send<TCustomer.TList, TCustomerQuery>(Query);
  try
    CustomersCtl.Items.BeginUpdate;
    try
      CustomersCtl.Items.Clear;
      for var C in Customers do
        with CustomersCtl.Items.Add do
        begin
          Caption := C.Name;
          SubItems.Add(C.Id.ToString);
        end;
    finally
      CustomersCtl.Items.EndUpdate;
    end;
  finally
    Customers.Free;
  end;
end;

procedure TMainForm.Reload;
begin
  LoadCustomers(FilterCtl.Text);
end;

procedure TMainForm.AddCtlClick(Sender: TObject);
begin
  // Validation runs in TValidationBehavior — no duplicate check here.
  // The list refreshes reactively via TFormCustomerAddedHandler.
  FMediator.Send(TAddCustomerCommand.Create(NameCtl.Text));
  NameCtl.Clear;
end;

procedure TMainForm.RemoveCtlClick(Sender: TObject);
begin
  if CustomersCtl.Selected = nil then
    Exit;
  var CustomerId := StrToInt(CustomersCtl.Selected.SubItems[0]);
  FMediator.Send(TRemoveCustomerCommand.Create(CustomerId));
end;

procedure TMainForm.SearchCtlClick(Sender: TObject);
begin
  LoadCustomers(FilterCtl.Text);
end;

procedure TMainForm.AllCtlClick(Sender: TObject);
begin
  FilterCtl.Clear;
  LoadCustomers;
end;

{ TFormCustomerAddedHandler }

procedure TFormCustomerAddedHandler.Handle(Notification: TCustomerAddedEvent);
begin
  inherited;
  MainForm.Reload;
end;

{ TFormCustomerRemovedHandler }

procedure TFormCustomerRemovedHandler.Handle(Notification: TCustomerRemovedEvent);
begin
  inherited;
  MainForm.Reload;
end;

end.
