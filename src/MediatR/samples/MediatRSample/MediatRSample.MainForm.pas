unit MediatRSample.MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.ComCtrls,
  Daf.MediatR.Contracts,
  MediatRSample.Requests,
  MediatRSample.Customer;

type
  TMainForm = class(TForm)
    pnlTop: TPanel;
    btnAddCustomer: TButton;
    edtCustomerName: TEdit;
    lblCustomerName: TLabel;
    lvCustomers: TListView;
    procedure btnAddCustomerClick(Sender: TObject);
  private
    FMediator: IMediator;
    procedure LoadCustomers;
    procedure HandleCustomerAdded(const Customer: TCustomer);
  public
    procedure Initialize(const Mediator: IMediator);
  end;

  TMainFormAddedEvent = class(TNotificacionHandler<TCustomerAddedEvent>)
  public
    procedure Handle(Notification: TCustomerAddedEvent);override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.Initialize(const Mediator: IMediator);
begin
  FMediator := Mediator;
  LoadCustomers;
end;

procedure TMainForm.LoadCustomers;
var
  Result: TCustomer.TList;
begin
  var Query := TCustomerQuery.Create;
  Result := FMediator.Send<TCustomer.TList, TCustomerQuery>(Query);
  try
    lvCustomers.Items.Clear;
    for var Customer in Result do
    begin
      with lvCustomers.Items.Add do
      begin
        Caption := Customer.Name;
        SubItems.Add(Customer.Id.ToString);
      end;
    end;
  finally
    Result.Free;
  end;
end;

procedure TMainForm.btnAddCustomerClick(Sender: TObject);
var
  Command: TAddCustomerCommand;
begin
  if Trim(edtCustomerName.Text).IsEmpty then
  begin
    ShowMessage('Por favor ingrese el nombre del cliente');
    Exit;
  end;

  Command := TAddCustomerCommand.Create(edtCustomerName.Text);
  FMediator.Send(Command);
  edtCustomerName.Clear;
  LoadCustomers;
end;

procedure TMainForm.HandleCustomerAdded(const Customer: TCustomer);
begin
  LoadCustomers;
end;

{ TMainFormAddedEvent }

procedure TMainFormAddedEvent.Handle(Notification: TCustomerAddedEvent);
begin
  inherited;
  MainForm.HandleCustomerAdded(Notification.Customer);
end;

end.
