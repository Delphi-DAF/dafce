unit DAF.Application.Builder;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  Daf.Extensions.Hosting,
  Daf.Extensions.DependencyInjection,
  Daf.Extensions.Configuration,
  Daf.Hosting,
  Daf.Application.Version,
  Daf.AppModule.DependencyInjection;

type
  IDafApplication = interface(IInvokable)
    ['{FAE0176F-FED1-42DF-92DF-C2A219853AC4}']
    function GetVersionInfo: TVersionInfo;
    function GetEnvironment: IHostEnvironment;
    function GetHost: IHost;
    function GetServices: IServiceProvider;
    procedure Start;
    procedure Stop;
    procedure WaitForShutdown;
    procedure Run(const WaitForShutdown: Boolean = False);overload;
    procedure Run(const Exec: TProc<IServiceProvider>; const WaitForShutdown: Boolean = False);overload;
    //Actually only for syntactic compatibility
    procedure RunAsync(const Exec: TProc<IServiceProvider> = nil; const WaitForShutdown: Boolean = False);
    function GetConfiguration: IConfiguration;
    property VersionInfo: TVersionInfo read GetVersionInfo;
    property Host: IHost read GetHost;
    property Environment: IHostEnvironment read GetEnvironment;
    property Configuration: IConfiguration read GetConfiguration;
    property Services: IServiceProvider read GetServices;
  end;

  TDafApplication = class;
  TDafApplicationClass = class of TDafApplication;
  TDafApplicationBuilderClass = class of TDafApplicationBuilder;
  TDafApplicationBuilder = class(THostBuilder)
  strict private
    FAppClass: TDafApplicationClass;
  private
  protected
    procedure DoConfigureServices(Context: IHostBuilderContext; Services: IServiceCollection);override;
  public
    function AppClass: TDafApplicationClass;inline;
    constructor Create(const AppClass: TDafApplicationClass);
    function Build: IDafApplication;
  end;

  {$M+}
  TDafApplication = class(TInterfacedObject, IDafApplication)
  strict private
    FHost: IHost;
    FEnvironment: IHostEnvironment;
    FConfiguration: IConfiguration;
  private
    class var FApp: TDafApplication;
    function GetEnvironment: IHostEnvironment;
    function GetServices: IServiceProvider;
    function GetHost: IHost;
    function GetConfiguration: IConfiguration;
    function GetVersionInfo: TVersionInfo;
  protected
    class function CreateHostBuilder(BuilderClass: TDafApplicationBuilderClass): TDafApplicationBuilder;overload;
    procedure Execute;virtual;
  public
    class function VersionInfo: TVersionInfo;virtual;
    class function VersionRequested: Boolean;virtual;
    class function CreateHostBuilder: TDafApplicationBuilder;overload;
    class function CreateHostBuilder<T: TDafApplicationBuilder>: T;overload;
    class function App: TDafApplication;inline;

    constructor Create(const Host: IHost);virtual;
    procedure Start;
    procedure Stop;
    procedure WaitForShutdown;
    procedure Run(const WaitForShutdown: Boolean = False);overload;
    procedure Run(const Exec: TProc<IServiceProvider>; const WaitForShutdown: Boolean = False);overload;
    //Actually only for syntactic compatibility
    procedure RunAsync(const Exec: TProc<IServiceProvider> = nil; const WaitForShutdown: Boolean = False);
    property Host: IHost read GetHost;
    property Environment: IHostEnvironment read GetEnvironment;
    property Configuration: IConfiguration read GetConfiguration;
    property Services: IServiceProvider read GetServices;
  end;
  {$M-}

implementation
uses
  Daf.Rtti,
  Daf.DependencyInjection;


{ TDafApplicationBuilder }

constructor TDafApplicationBuilder.Create(const AppClass: TDafApplicationClass);
begin
  inherited Create;
  FAppClass := AppClass;
end;


procedure TDafApplicationBuilder.DoConfigureServices(Context: IHostBuilderContext; Services: IServiceCollection);
begin
  inherited;
  // El descubrimiento sigue siendo el comportamiento por defecto de una TDafApplication
  // hasta 2.0; el aviso es para quien lo escriba en su propio arranque, no para DAF.
{$WARN SYMBOL_DEPRECATED OFF}
  AppModules.Add(Services, _T.PackageOf(FAppClass));
{$WARN SYMBOL_DEPRECATED ON}
end;

function TDafApplicationBuilder.AppClass: TDafApplicationClass;
begin
  Result := FAppClass;
end;

function TDafApplicationBuilder.Build: IDafApplication;
begin
  Self._AddRef;
  try
    var Host := inherited Build;
    Result := FAppClass.Create(Host);
  finally
    Self._Release;;
  end;
end;

{ TDafApplication }

class function TDafApplication.VersionRequested: Boolean;
begin
  Result := (ParamCount > 0) and (ParamStr(1) = '--version');
end;

class function TDafApplication.VersionInfo: TVersionInfo;
begin
  Result := TVersionInfo.GetFrom(Self);
end;

class function TDafApplication.CreateHostBuilder(BuilderClass: TDafApplicationBuilderClass): TDafApplicationBuilder;
begin
  Result := BuilderClass.Create(Self);
end;

class function TDafApplication.CreateHostBuilder: TDafApplicationBuilder;
begin
  Result := CreateHostBuilder(TDafApplicationBuilder);
end;

class function TDafApplication.CreateHostBuilder<T>: T;
begin
  Result := CreateHostBuilder(T) as T;
end;

constructor TDafApplication.Create(const Host: IHost);
begin
  FApp := Self;
  inherited Create;
  FHost := Host;
  FEnvironment := Services.GetRequiredService<IHostEnvironment>;
  FConfiguration := Services.GetRequiredService<IConfiguration>;
end;

procedure TDafApplication.Run(const WaitForShutdown: Boolean = False);
begin
  RunAsync(nil, WaitForShutdown);
end;

procedure TDafApplication.Run(const Exec: TProc<IServiceProvider>; const WaitForShutdown: Boolean = False);
begin
  RunAsync(Exec, WaitForShutdown);
end;

procedure TDafApplication.RunAsync(const Exec: TProc<IServiceProvider> = nil; const WaitForShutdown: Boolean = False);
begin
  Start;
  try
    if Assigned(Exec) then
      Exec(Services)
    else
      Execute;
  except
    Stop;
    raise;
  end;

  if WaitForShutdown then
    Host.WaitForShutdown;
  Stop;
end;

procedure TDafApplication.Execute;
begin

end;

procedure TDafApplication.Start;
begin
  Host.Start;
end;

procedure TDafApplication.Stop;
begin
  Host.Stop;
  FHost := nil;
end;

procedure TDafApplication.WaitForShutdown;
begin
  Host.WaitForShutdown;
end;

function TDafApplication.GetVersionInfo: TVersionInfo;
begin
  Result := VersionInfo;
end;

class function TDafApplication.App: TDafApplication;
begin
  Result := FApp;
end;

function TDafApplication.GetHost: IHost;
begin
  Result := FHost;
end;

function TDafApplication.GetConfiguration: IConfiguration;
begin
  Result := FConfiguration;
end;

function TDafApplication.GetEnvironment: IHostEnvironment;
begin
  Result := FEnvironment;
end;

function TDafApplication.GetServices: IServiceProvider;
begin
  Result := FHost.Services;
end;

end.

