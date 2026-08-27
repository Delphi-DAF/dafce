unit DAF.NNLog.Loader;

interface

uses
  Daf.Extensions.Hosting,
  Daf.Extensions.Logging,
  DAF.Logging.Provider,
  DAF.NNLog,
  DAF.NNLog.Layout,
  DAF.NNLog.Configuration,
  DAF.Extensions.Configuration,
  DAF.Configuration.Binder,
  System.Generics.Collections;

type
  TNNLogLoader = class
  private
    class function BuildRule(const TargetMap: TDictionary<string, TTarget>; const RuleCfg: TLogRuleConfig; const Index: Integer): TLogRule; static;
    class function BuildTarget(const TargetCfg: TTargetConfig): TTarget; static;
    class function ParseWriteTo(const TargetMap: TDictionary<string, TTarget>; const WriteTo: string): TArray<TTarget>; static;
  public
    class function Load(const Config: IConfiguration): TNNLogConfiguration;
    class function BuildProvider(const Context: IHostBuilderContext; const ConfigKey: string): ILoggerProvider;overload;
    class function BuildProvider(const Environment: IHostEnvironment; const Config: IConfiguration): ILoggerProvider;overload;
  end;

implementation

uses
  System.Classes,
  System.IOUtils,
  System.Rtti,
  System.RegularExpressions,
  System.RegularExpressionsCore,
  System.SysUtils,
  System.TypInfo;

{ TNNLogLoader }

class function TNNLogLoader.BuildTarget(const TargetCfg: TTargetConfig): TTarget;
begin

  var TargetType := TargetCfg.TargetType;
  var RttiContext := TRttiContext.Create;

  // Naming convenion: Targets live in DAF.NNLog.Targets.<TargetType>
  // The class must be named T<TargetType>Target;
  // WARINING: FindType is Case Sensitive !!
  var FullTargetClassName := Format('DAF.NNLog.Targets.%s.T%sTarget', [TargetType, TargetType]);
  if TargetType = 'File' then //is reserved word and cannot follow the convenion
    FullTargetClassName := Format('DAF.NNLog.Targets.%s_.T%sTarget', [TargetType, TargetType]);

  var RttiType := RttiContext.FindType(FullTargetClassName);

  if not Assigned(RttiType) then
    raise Exception.CreateFmt('DAF.NNLog Error: Unknown target type %s', [TargetType]);

  var TargetClass := TTargetClass(RttiType.AsInstance.MetaclassType);

  if  not TargetClass.InheritsFrom(TTarget) then
    raise Exception.CreateFmt('DAF.NNLog Error: Invalid target type %s', [TargetType]);

  Result := TargetClass.Create as TTarget;
  Result.Name := TargetCfg.Name;
  for var Key in TargetCfg.Keys do
  begin
    var Prop := RttiType.GetProperty(Key);
    if Assigned(Prop) and Prop.IsWritable then
      Prop.SetValue(Result, ConvertTo(TargetCfg[Key], Prop.DataType.Handle));
  end;
end;

class function TNNLogLoader.ParseWriteTo(const TargetMap: TDictionary<string, TTarget>; const WriteTo: string): TArray<TTarget>;
begin
  var Names := WriteTo.Split([',']);
  SetLength(Result, 0);
  for var Name in Names do
  begin
    if TargetMap.ContainsKey(Name) then
      Result := Result + [TargetMap[Name]]
  end;
end;

class function TNNLogLoader.BuildRule(const TargetMap: TDictionary<string, TTarget>; const RuleCfg: TLogRuleConfig; const Index: Integer): TLogRule;
begin
  Result := TLogRule.Create;
  try
    // enabled se resuelve como sus tres vecinos (minLevel, maxLevel, levels): una cadena que
    // se interpreta con su defecto. El defecto es ACTIVADA --lo que dice NLog y la cabecera
    // del modulo--: solo el literal `false` la desactiva (D1).
    Result.Enabled := not SameText(Trim(RuleCfg.Enabled), 'false');
    Result.MinLevel := TLogLevel.Parse(RuleCfg.MinLevel, Low(TLogLEvel));
    Result.Levels :=  TLogLevel.ParseList(RuleCfg.Levels);
    Result.MaxLevel := TLogLevel.Parse(RuleCfg.MaxLevel, Pred(High(TLogLevel)));
    Result.WriteTo :=  ParseWriteTo(TargetMap, RuleCfg.WriteTo);
    Result.IsFinal := RuleCfg.IsFinal;
    // El patron se compila al asignar Name. Un patron que no se puede interpretar es un error
    // de composicion y se dice al cargar, nombrando la regla y el patron (D4).
    try
      Result.Name := RuleCfg.Name;
    except
      on E: ERegularExpressionError do
        raise Exception.CreateFmt('DAF.NNLog Error: rule #%d has an invalid category pattern "%s": %s',
          [Index, RuleCfg.Name, E.Message]);
    end;
  except
    Result.Free;
    raise;
  end;
end;

procedure Dump(Title: string; const Config: IConfiguration; const Indent: string = '');
begin
  if not Title.IsEmpty then
  begin
    WriteLn('-----------------');
    WriteLn(Title);
    WriteLn('-----------------');
  end;

  for var section in Config.GetChildren do
  begin
    if Section.HasChildren then
    begin
      Writeln(Indent + section.Key + ':');
      Dump('', section, Indent + '  ');
    end
    else
      Writeln(Indent + section.Key + ' = ' + section.Value);
  end;
end;

class function TNNLogLoader.Load(const Config: IConfiguration): TNNLogConfiguration;
begin
//  Dump('NNlog config', Config);
  Result := TConfigurationBinder.Bind<TNNLogConfiguration>(Config, '', boFields);
end;

class function TNNLogLoader.BuildProvider(const Context: IHostBuilderContext; const ConfigKey: string): ILoggerProvider;
begin
  Result := BuildProvider(Context.Environment, Context.Configuration.GetSection(ConfigKey));
end;

class function TNNLogLoader.BuildProvider(const Environment: IHostEnvironment; const Config: IConfiguration): ILoggerProvider;
begin
  TLogLayoutEngine.RegisterLayoutRenderers(Environment);
  var Rules: TArray<TLogRule>;
  var NLogConfig := Load(Config);
  try
    var TargetMap := TDictionary<string, TTarget>.Create;
    try
      try
        for var TgtCfg in NLogConfig.Targets do
          TargetMap.Add(TgtCfg.Key, BuildTarget(TgtCfg.Value));

        SetLength(Rules, Length(NLogConfig.Rules));
        var idx := 0;
        for var RuleCfg in NLogConfig.Rules do
        begin
          Rules[idx] := BuildRule(TargetMap, RuleCfg, idx);
          Inc(idx);
        end;
        Result := TNNLogProvider.Create(TargetMap.Values.ToArray, Rules);
      except
        // Composicion fallida (p. ej. un patron imposible): lo ya construido no tiene todavia
        // proveedor que lo posea, asi que se libera aqui para no fugarlo.
        for var R in Rules do
          if Assigned(R) then
            R.Free;
        for var T in TargetMap.Values do
          T.Free;
        raise;
      end;
    finally
      TargetMap.Free;
    end;
  finally
    NLogConfig.Free;
  end;
end;

end.

