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
    pnlTop: TPanel;
    lblName: TLabel;
    edtName: TEdit;
    btnAdd: TButton;
    btnRemove: TButton;
    lblFilter: TLabel;
    edtFilter: TEdit;
    btnSearch: TButton;
    btnAll: TButton;
    lvCustomers: TListView;
    grpLog: TGroupBox;
    memoLog: TMemo;
    procedure btnAddClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure btnSearchClick(Sender: TObject);
    procedure btnAllClick(Sender: TObject);
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
    memoLog.Lines.Add(Msg);
    SendMessage(memoLog.Handle, EM_SCROLL, SB_BOTTOM, 0);
  end);
  LoadCustomers;
end;

procedure TMainForm.LoadCustomers(const FilterText: string = '');
var
  Query: TCustomerQuery;
  Customers: TCustomer.TList;
begin
  if FilterText.IsEmpty then
    Query := TCustomerQuery.Create
  else
    Query := TCustomerQuery.Create(
      function(C: TCustomer): Boolean
      begin
        Result := C.Name.ToLower.Contains(FilterText.ToLower);
      end);

  Customers := FMediator.Send<TCustomer.TList, TCustomerQuery>(Query);
  try
    lvCustomers.Items.BeginUpdate;
    try
      lvCustomers.Items.Clear;
      for var C in Customers do
        with lvCustomers.Items.Add do
        begin
          Caption := C.Name;
          SubItems.Add(C.Id.ToString);
        end;
    finally
      lvCustomers.Items.EndUpdate;
    end;
  finally
    Customers.Free;
  end;
end;

procedure TMainForm.Reload;
begin
  LoadCustomers(edtFilter.Text);
end;

procedure TMainForm.btnAddClick(Sender: TObject);
begin
  // Validation runs in TValidationBehavior — no duplicate check here.
  // The list refreshes reactively via TFormCustomerAddedHandler.
  FMediator.Send(TAddCustomerCommand.Create(edtName.Text));
  edtName.Clear;
end;

procedure TMainForm.btnRemoveClick(Sender: TObject);
begin
  if lvCustomers.Selected = nil then
    Exit;
  var CustomerId := StrToInt(lvCustomers.Selected.SubItems[0]);
  FMediator.Send(TRemoveCustomerCommand.Create(CustomerId));
end;

procedure TMainForm.btnSearchClick(Sender: TObject);
begin
  LoadCustomers(edtFilter.Text);
end;

procedure TMainForm.btnAllClick(Sender: TObject);
begin
  edtFilter.Clear;
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
