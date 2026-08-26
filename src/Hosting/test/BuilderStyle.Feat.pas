unit BuilderStyle.Feat;

interface

type
  // In the interface section by obligation, not by taste: the container resolves through
  // RTTI, and a type declared only in the implementation section is not there to be found.
  IMarker = interface(IInvokable)
    ['{6C1C4C2B-6F1E-4C61-9E63-1E5F3A4B7D21}']
    function Name: string;
  end;

  TMarker = class(TInterfacedObject, IMarker)
  private
    FName: string;
  public
    constructor Create(const AName: string);
    function Name: string;
  end;

  TOverlapWorld = class
  public
    Report: string;
  end;

implementation

uses
  System.SysUtils,
  Daf.MiniSpec,
  Daf.Extensions.DependencyInjection,
  Daf.Extensions.Hosting,
  Daf.Hosting;

constructor TMarker.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
end;

function TMarker.Name: string;
begin
  Result := FName;
end;

// --- Feature definition ---

initialization

Feature('''
Feature Several startups in one process @hosting @builder-style

  As a spec author, and as anyone whose startup now lives outside Build
  I want two builders and two hosts to be alive at the same time in one process
  So that state belonging to one of them never reaches the other
''')

.UseWorld<TOverlapWorld>

.Rule('Overlapping startups')

  // The uniqueness this used to assert was a class var, so a second builder created before
  // the first was gone tripped it. With the immediate style that stops being the odd case:
  // what used to be born and die inside Build now lives for the whole startup.
  .Scenario('Two builders alive at once')
    .Given('one builder that is not going to be released',
      procedure(W: TOverlapWorld)
      begin
        var First: IHostBuilder := THostBuilder.Create;
        First.ConfigureServices(
          procedure(Context: IHostBuilderContext; Services: IServiceCollection)
          begin
            Services.AddSingleton<IMarker>(TMarker.Create('first') as IMarker);
          end);

        // Created while the first one is still held: two builders, two environments and two
        // builder contexts alive at the same time.
        var Second: IHostBuilder := THostBuilder.Create;
        Second.ConfigureServices(
          procedure(Context: IHostBuilderContext; Services: IServiceCollection)
          begin
            Services.AddSingleton<IMarker>(TMarker.Create('second') as IMarker);
          end);

        // Both hosts alive at the same time, not one after the other. Stop is what releases
        // the container -- and with it the host -- so both are stopped in the finally; a host
        // that is built and dropped keeps its provider alive and the leak report says so.
        var FirstHost := First.Build;
        var SecondHost := Second.Build;
        FirstHost.Start;
        SecondHost.Start;
        try
          W.Report := FirstHost.Services.GetService<IMarker>.Name + ',' +
                      SecondHost.Services.GetService<IMarker>.Name;
        finally
          FirstHost.Stop;
          SecondHost.Stop;
        end;
      end)
    .When('both are built and asked what they registered').NoAction
    .&Then('each one answers with its own, and no uniqueness assertion fired',
      procedure(W: TOverlapWorld)
      begin
        Expect(W.Report).ToEqual('first,second');
      end)

  .Scenario('A suite starts and stops several hosts')
    .Given('three hosts built one after another in the same process',
      procedure(W: TOverlapWorld)
      begin
        W.Report := '';
        for var I := 1 to 3 do
        begin
          var Builder: IHostBuilder := THostBuilder.Create;
          var Tag := 'h' + I.ToString;
          Builder.ConfigureServices(
            procedure(Context: IHostBuilderContext; Services: IServiceCollection)
            begin
              Services.AddSingleton<IMarker>(TMarker.Create(Tag) as IMarker);
            end);
          var Host := Builder.Build;
          Host.Start;
          try
            W.Report := W.Report + Host.Services.GetService<IMarker>.Name;
          finally
            Host.Stop;
          end;
        end;
      end)
    .When('each one is started and stopped in turn').NoAction
    .&Then('every one of them saw its own state',
      procedure(W: TOverlapWorld)
      begin
        Expect(W.Report).ToEqual('h1h2h3');
      end);

end.
