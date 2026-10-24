unit StaticFiles.Feat;

{
  The router serves a file before it tries any route, so what it is willing to serve is decided
  here: nothing until the application declares a root, and from a root only what lies inside it.
}

interface

uses
  Daf.Extensions.DependencyInjection,
  Daf.DependencyInjection,
  Daf.Web.Router,
  Web.Doubles;

type
  TStaticWorld = class
  public
    Services: IServiceCollection;
    Provider: IServiceProvider;
    Router: TWebRouter;
    RouterRef: IWebRouter;
    Request: TSpecWebRequest;
    Response: TSpecWebResponse;
    Handled: Boolean;
    /// <summary>The scenario's own folder: the root and the files outside it live under it.</summary>
    Base: string;
    PriorDir: string;
    Secret: string;
    /// <summary>What the response carried, or the mark that nothing was served.</summary>
    Served: string;
    constructor Create;
    destructor Destroy; override;
    procedure DispatchGet(const Path: string);
    function WriteFile(const Path, Content: string): string;
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  Daf.MiniSpec;

const
  NothingServed = '<nothing served>';

{ TStaticWorld }

constructor TStaticWorld.Create;
begin
  inherited;
  Services := TServiceCollection.Create;
  Provider := Services.BuildServiceProvider;
  Router := TWebRouter.Create(Provider.GetRequiredService<IServiceScopeFactory>);
  RouterRef := Router;
  Base := TPath.Combine(TPath.GetTempPath, 'DafStaticFiles-' + TGUID.NewGuid.ToString.Trim(['{', '}']));
  TDirectory.CreateDirectory(Base);
  PriorDir := GetCurrentDir;
end;

destructor TStaticWorld.Destroy;
begin
  // The response holds the served file open: it goes before the folder does.
  Response.Free;
  Request.Free;
  SetCurrentDir(PriorDir);
  if TDirectory.Exists(Base) then
    TDirectory.Delete(Base, True);
  RouterRef := nil;
  Provider.ShutDown;
  Services := nil;
  inherited;
end;

function TStaticWorld.WriteFile(const Path, Content: string): string;
begin
  TDirectory.CreateDirectory(TPath.GetDirectoryName(Path));
  TFile.WriteAllText(Path, Content, TEncoding.ASCII);
  Result := Path;
end;

procedure TStaticWorld.DispatchGet(const Path: string);
begin
  Response.Free;
  Request.Free;
  Request := TSpecWebRequest.Create('GET', Path);
  Response := TSpecWebResponse.Create(Request);
  Handled := False;
  Router.WebDispatch(nil, Request, Response, Handled);
  if Response.ContentStream = nil then
    Served := NothingServed
  else
  begin
    var Reader := TStreamReader.Create(Response.ContentStream, TEncoding.ASCII);
    try
      Served := Reader.ReadToEnd;
    finally
      Reader.Free;
    end;
  end;
end;

initialization

Feature('''
Feature Static files @web @static

  As an application serving HTTP
  I want the router to serve files only from the root I declare
  So nothing else the process can read reaches a client
''')

.UseWorld<TStaticWorld>

.Rule('With no root declared, no file is served')

  .Scenario('A file in the working directory is not served')
    .Given('a router with no root, and a file in the working directory',
      procedure(W: TStaticWorld)
      begin
        SetCurrentDir(W.Base);
        W.WriteFile(TPath.Combine(W.Base, 'settings.json'), 'secret');
        Expect(W.RouterRef.RootDirectory).ToEqual('');
        Expect(TFile.Exists('settings.json')).ToBeTrue;
      end)
    .When('a GET arrives whose path is the name of that file',
      procedure(W: TStaticWorld)
      begin
        W.DispatchGet('/settings.json');
      end)
    .&Then('the router does not answer with the file',
      procedure(W: TStaticWorld)
      begin
        Expect(W.Served).ToEqual(NothingServed);
      end)

.Rule('With a root declared, only what lies inside it is served')

  .Scenario('A file under the root is served')
    .Given('a router whose root holds a file',
      procedure(W: TStaticWorld)
      begin
        var Root := TPath.Combine(W.Base, 'root');
        W.WriteFile(TPath.Combine(Root, 'page.txt'), 'hello');
        W.RouterRef.RootDirectory := Root;
      end)
    .When('a GET arrives with the path of that file relative to the root',
      procedure(W: TStaticWorld)
      begin
        W.DispatchGet('/page.txt');
      end)
    .&Then('the response carries the file',
      procedure(W: TStaticWorld)
      begin
        Expect(W.Served).ToEqual('hello');
      end)

  .Scenario('Climbing with .. does not leave the root')
    .Given('a router whose root has a file in its parent folder',
      procedure(W: TStaticWorld)
      begin
        var Root := TPath.Combine(W.Base, 'root');
        TDirectory.CreateDirectory(Root);
        W.WriteFile(TPath.Combine(W.Base, 'secret.txt'), 'secret');
        W.RouterRef.RootDirectory := Root;
      end)
    .When('a GET arrives whose path climbs to that file with ..',
      procedure(W: TStaticWorld)
      begin
        W.DispatchGet('/../secret.txt');
      end)
    .&Then('the router does not answer with the file',
      procedure(W: TStaticWorld)
      begin
        Expect(W.Served).ToEqual(NothingServed);
      end)

  .Scenario('An absolute path does not leave the root')
    .Given('a router with a root, and a file outside it',
      procedure(W: TStaticWorld)
      begin
        var Root := TPath.Combine(W.Base, 'root');
        TDirectory.CreateDirectory(Root);
        W.Secret := W.WriteFile(TPath.Combine(W.Base, 'secret.txt'), 'secret');
        W.RouterRef.RootDirectory := Root;
      end)
    .When('a GET arrives whose path is the absolute path of that file',
      procedure(W: TStaticWorld)
      begin
        W.DispatchGet('/' + W.Secret.Replace('\', '/'));
      end)
    .&Then('the router does not answer with the file',
      procedure(W: TStaticWorld)
      begin
        Expect(W.Served).ToEqual(NothingServed);
      end);

end.
