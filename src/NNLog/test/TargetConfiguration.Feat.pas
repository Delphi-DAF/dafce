unit TargetConfiguration.Feat;

interface

uses
  Daf.Extensions.Logging,
  Daf.Extensions.Hosting;

type
  TTargetConfigWorld = class
  strict private
    FSavedSinks: TArray<IBootstrapSink>;
    FSavedDecimal: Char;
    FSavedThousand: Char;
    FProvider: ILoggerProvider;
    FPairs: TArray<string>;
  public
    /// Lo que el canal de arranque recibió mientras se cargaba la configuración.
    Reported: string;
    constructor Create;
    destructor Destroy; override;
    procedure Configure(const Key, Value: string);
    procedure Load;
  end;

implementation

uses
  System.SysUtils,
  System.Generics.Collections,
  Daf.Extensions.Configuration,
  Daf.Configuration.Builder,
  Daf.Configuration.Memory,
  Daf.Hosting,
  Daf.NNLog.Loader,
  Daf.NNLog.Targets.Probe,
  Daf.MiniSpec;

type
  TCollectingSink = class(TInterfacedObject, IBootstrapSink)
  strict private
    FWorld: TTargetConfigWorld;
  public
    constructor Create(World: TTargetConfigWorld);
    function IsAvailable: Boolean;
    procedure Write(const Line: string);
  end;

{ TCollectingSink }

constructor TCollectingSink.Create(World: TTargetConfigWorld);
begin
  inherited Create;
  FWorld := World;
end;

function TCollectingSink.IsAvailable: Boolean;
begin
  Result := True;
end;

procedure TCollectingSink.Write(const Line: string);
begin
  FWorld.Reported := FWorld.Reported + Line + sLineBreak;
end;

{ TTargetConfigWorld }

constructor TTargetConfigWorld.Create;
begin
  inherited;
  FSavedDecimal := FormatSettings.DecimalSeparator;
  FSavedThousand := FormatSettings.ThousandSeparator;
  FSavedSinks := BootstrapLog.Sinks;
  BootstrapLog.ClearSinks;
  BootstrapLog.AddSink(TCollectingSink.Create(Self));
  TProbeTarget.Last := nil;
end;

destructor TTargetConfigWorld.Destroy;
begin
  FProvider := nil;
  BootstrapLog.ClearSinks;
  for var Sink in FSavedSinks do
    BootstrapLog.AddSink(Sink);
  FormatSettings.DecimalSeparator := FSavedDecimal;
  FormatSettings.ThousandSeparator := FSavedThousand;
  inherited;
end;

procedure TTargetConfigWorld.Configure(const Key, Value: string);
begin
  FPairs := FPairs + [Key, Value];
end;

procedure TTargetConfigWorld.Load;
begin
  var Dict := TDictionary<string, string>.Create;
  Dict.AddOrSetValue('Targets:probe:TargetType', 'Probe');
  Dict.AddOrSetValue('Targets:probe:Name', 'probe');
  var I := 0;
  while I < High(FPairs) do
  begin
    Dict.AddOrSetValue(FPairs[I], FPairs[I + 1]);
    Inc(I, 2);
  end;
  var Builder: IConfigurationBuilder := TConfigurationBuilder.Create;
  // La fuente en memoria pasa a poseer el diccionario.
  MemoryConfig.AddCollection(Builder, Dict);
  var Config: IConfiguration := Builder.Build as IConfiguration;
  var EnvBuilder: IConfigurationBuilder := TConfigurationBuilder.Create;
  var Env := THostEnvironment.Create(EnvBuilder.Build as IConfiguration) as IHostEnvironment;
  FProvider := TNNLogLoader.BuildProvider(Env, Config);
end;

initialization

Feature('''
Feature Una propiedad de destino que no convierte se informa @nnlog @targets @configuration

  Como quien configura los destinos de NNLog en un fichero
  Quiero que un valor que no sirve para su propiedad se avise y la deje como estaba
  Para no descubrir un destino mal configurado por lo que dejó de escribir
''')

.UseWorld<TTargetConfigWorld>

.Rule('Una propiedad de destino que no convierte se informa y conserva su valor')

  .Scenario('Un tamaño que no es un número')
    .Given('una configuración de NNLog con un destino de prueba cuyo tamaño máximo es abc',
      procedure(W: TTargetConfigWorld)
      begin
        W.Configure('Targets:probe:MaxSize', 'abc');
      end)
    .When('se carga la configuración',
      procedure(W: TTargetConfigWorld)
      begin
        W.Load;
      end)
    .&Then('el destino existe y su tamaño máximo es el que tenía por defecto',
      procedure(W: TTargetConfigWorld)
      begin
        Expect(Assigned(TProbeTarget.Last)).ToBeTrue;
        Expect(TProbeTarget.Last.MaxSize).ToEqual(1024);
      end)
    .&And('el canal de arranque recibió un aviso que nombra el destino, la propiedad y abc',
      procedure(W: TTargetConfigWorld)
      begin
        Expect(W.Reported).ToContain('''probe''');
        Expect(W.Reported).ToContain('''MaxSize''');
        Expect(W.Reported).ToContain('''abc''');
      end)

  .Scenario('Un decimal en un proceso con coma decimal')
    .Given('un proceso cuyo formato usa la coma decimal, y una propiedad decimal de un destino de prueba escrita con punto',
      procedure(W: TTargetConfigWorld)
      begin
        FormatSettings.DecimalSeparator := ',';
        FormatSettings.ThousandSeparator := '.';
        W.Configure('Targets:probe:Ratio', '1.5');
      end)
    .When('se carga la configuración',
      procedure(W: TTargetConfigWorld)
      begin
        W.Load;
      end)
    .&Then('la propiedad tiene ese valor, y el canal de arranque no recibió ningún aviso',
      procedure(W: TTargetConfigWorld)
      begin
        Expect(Format('Ratio=%s avisos=[%s]', [FloatToStr(TProbeTarget.Last.Ratio, TFormatSettings.Invariant),
          W.Reported])).ToEqual('Ratio=1.5 avisos=[]');
      end);

end.
