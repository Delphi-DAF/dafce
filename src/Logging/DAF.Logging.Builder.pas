unit DAF.Logging.Builder;

interface

uses
  System.Generics.Collections,
  System.SysUtils,
  DAF.Extensions.DependencyInjection,
  Daf.Extensions.Logging,
  DAF.Logging.Provider;

type
  ILoggingBuilder = interface(IInvokable)
    ['{F989B61A-8A02-4C1C-AF3D-DA94CF097B1C}']
    procedure AddProvider(const Provider: ILoggerProvider);
    function Build: ILoggerFactory;
  end;

  TLoggingBuilder = class(TInterfacedObject, ILoggingBuilder)
  private
    FFactory: TMultiProviderLoggerFactory;
  public
    constructor Create;
    procedure AddProvider(const Provider: ILoggerProvider);
    function Build: ILoggerFactory;
  end;

procedure AddLogging(const Services: IServiceCollection; const Add: TProc<ILoggingBuilder>);

implementation

{ TLoggingBuilder }

constructor TLoggingBuilder.Create;
begin
  inherited Create;
  FFactory := TMultiProviderLoggerFactory.Create;
end;

procedure TLoggingBuilder.AddProvider(const Provider: ILoggerProvider);
begin
  FFactory.AddProvider(Provider);
end;

function TLoggingBuilder.Build: ILoggerFactory;
begin
  Result := FFactory;
end;

procedure AddLogging(const  Services: IServiceCollection; const Add: TProc<ILoggingBuilder>);
var
  Builder: ILoggingBuilder;
begin
  Builder := TLoggingBuilder.Create;
  Add(Builder);
  Services.AddSingleton<ILoggerFactory>(Builder.Build);
end;

end.
