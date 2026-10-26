unit Daf.NNLog.Configuration;

interface

uses
  System.Diagnostics,
  System.Generics.Collections,
  System.SysUtils;

type
  {
  (From https://github.com/nlog/NLog/wiki/Configuration-file#rules)

  A single rule is defined with a logger element that filters what Logger-objects to match:

  name – Match logger-name of the Logger-object - may include wildcard characters (* and ?), In Daf we use Logger.Category as NNLog logger name
  minlevel – minimum level to log (matches the specified level and levels above)
  maxlevel – maximum level to log (matches the specified level and levels below)
  levels - comma separated list of levels to log
  writeTo – comma separated list of targets to write to
  final – no rules are processed after a final rule matches
  enabled - set to false to discard the logging rule completely, and reload is required to re-enable. Please note only literal true or false values can be used here; variables cannot be used to set this property.
}
  {$M+}
  TLogRuleConfig = class
  public
    Name: string;
    Enabled: Boolean;
    MinLevel: string;
    Levels: string; // comma separate  list
    MaxLevel: string;
    WriteTo: string; // comma separate list
    IsFinal: Boolean;
    /// <summary>
    ///   Los defectos de una regla que la configuracion no menciona. Enabled arranca en True
    ///   porque una regla sin `enabled` esta activada; IsFinal arranca en False, que ademas de
    ///   ser su defecto correcto es el cero de su tipo, asi que no hace falta escribirlo.
    /// </summary>
    constructor Create;
  end;

  TTargetConfig = class(TDictionary<string, string>)
  private
    function GetName: string;
    function GetTargetType: string;
  public
    property Name: string read GetName;
    property TargetType: string read GetTargetType;
    constructor Create;
  end;

  TNNLogConfiguration = class
  public
    Targets: TObjectDictionary<string, TTargetConfig>;
    Rules: TArray<TLogRuleConfig>;
    constructor Create;
    destructor Destroy;override;
  end;
  {$M-}

implementation
uses System.Generics.Defaults;

{ TLogRuleConfig }

constructor TLogRuleConfig.Create;
begin
  inherited Create;
  Enabled := True;
end;

{ TTargetConfig }

constructor TTargetConfig.Create;
begin
  inherited Create(TIStringComparer.Ordinal);
end;

function TTargetConfig.GetName: string;
begin
  Self.TryGetValue('Name', Result);
end;

function TTargetConfig.GetTargetType: string;
begin
  Self.TryGetValue('TargetType', Result);
end;

{ TNNLogConfiguration }

constructor TNNLogConfiguration.Create;
begin
  inherited;
  Targets := TObjectDictionary<string, TTargetConfig>.Create([doOwnsValues]);
end;

destructor TNNLogConfiguration.Destroy;
begin
  for var Item in Rules do
    Item.Free;
  Targets.Free;
  inherited;
end;

end.

