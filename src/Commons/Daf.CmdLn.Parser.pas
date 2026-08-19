unit Daf.CmdLn.Parser;

interface

uses
  System.Generics.Collections,
  System.RegularExpressions,
  System.SysUtils,
  System.TypInfo,
  System.Rtti;

type
  ICmdLParams = interface
    ['{AC487960-6057-4507-9048-FCF229372140}']
    function GetParent: ICmdLParams;
    function GetKey: string;
    function GetValue: TValue;
    procedure SetValue(const Value: TValue);
    function GetItem(Key: string): TValue;
    procedure SetItem(Key: string; const Value: TValue);
    function GetCommand: string;
    function HasItem(Key: string): Boolean;
    function GetSections: TEnumerable<ICmdLParams>;
    function GetSection(Key: string): ICmdLParams;
    function HasSections: Boolean;
    property Parent: ICmdLParams read GetParent;
    property Key: string read GetKey;
    property Command: string read GetCommand;
    property Value: TValue read GetValue write SetValue;
    property Item[Key: string]: TValue read GetItem write SetItem;default;
  end;

  TParserNode = class;
  TParserNodes<T: TParserNode> = class(TObjectList<T>);
  TParserNodes = TParserNodes<TParserNode>;

  TArgNode = class;
  TCmdNode = class;
  CmdLnParserError = class(Exception);
  TCmdLnParserOpts = record
    QuoteChar: Char;
    LongArgPrefix: string;
    ShortArgPrefix: string;
    class operator Initialize (out Dest: TCmdLnParserOpts);
  end;

  ICmdLnParser = interface
    ['{AD5AC093-1458-4142-B83A-A9DD7C64E8E3}']
    function GetCurrent: string;
    function GetOptions: TCmdLnParserOpts;
    function GetRoot: TParserNode;

    property Options: TCmdLnParserOpts read GetOptions;
    property Root: TParserNode read GetRoot;

    property Current: string read GetCurrent;
    function MoveNext: Boolean;
    function InBounds: Boolean;

    function Consume(const Nodes: TParserNodes): Boolean;overload;
    function Consume(const Node: TParserNode): Boolean;overload;
    function Consume(const Pattern: string): Boolean;overload;
    function Consume(const Pattern: string; out Value: string): Boolean;overload;
    function Parse(const CmdLine: string): ICmdLParams;
  end;

  TParserNode = class abstract
  strict private
    FName: string;
    FParent: TParserNode;
    FRequired: Boolean;
    FChildren: TParserNodes;
    function GetIsRoot: Boolean;
    function GetName: string;
    function GetChild(Name: string): TParserNode;
  protected
    procedure SetParent(const Value: TParserNode);
    function DoConsume(const Parser: ICmdLnParser): Boolean; virtual;abstract;

    function AddChild(const Node: TParserNode): TParserNode;
    function RemoveChild(const Node: TParserNode): TParserNode;

    procedure BeforeAdd(const Node: TParserNode);virtual;//TCollectionNotification
    procedure DoAddChild(const Node: TParserNode);virtual;
    procedure AfterAdd(const Node: TParserNode);virtual;

    procedure BeforeRemove(const Node: TParserNode);virtual;//TCollectionNotification
    procedure DoRemoveChild(const Node: TParserNode);virtual;
    procedure AfterRemove(const Node: TParserNode);virtual;
  public
    class function Error(FmtStr: string; Args: array of const): CmdLnParserError;overload;
    class function Error(Msg: string): CmdLnParserError;overload;
    constructor Create(const Name: string = ''; const Parent: TParserNode = nil);
    destructor Destroy;override;
{$REGION 'Fluent API'}
    function Required(const Value: Boolean): TParserNode;overload;
    function Required: Boolean;overload;
    property Name: string read GetName;
    property IsRoot: Boolean read GetIsRoot;
    property Parent: TParserNode read FParent;
    property Child[Name: string]: TParserNode read GetChild;default;
    property Children: TParserNodes read FChildren;
  end;

  TRepeatableNode = class(TParserNode)
  type
    TValidator = TFunc<TRepeatableNode,Boolean>;
  private
    FConsumedNodes: TParserNodes;
    FValidations: TArray<TValidator>;
  protected
    function ConsumeSomeNode(const Parser: ICmdLnParser): Boolean;
    function ConsumedIsValid: Boolean;
    function DoConsume(const Parser: ICmdLnParser): Boolean; override;
    class function RequiredsAreConsumed(Validated: TRepeatableNode): Boolean;
    class function ConsumedIsNotEmpty(Validated: TRepeatableNode): Boolean; static;
  public
    constructor Create(const Name: string = ''; const Parent: TParserNode = nil);
    destructor Destroy;override;
    procedure AddValidation(const Validator: TValidator);
  end;

  TArgSetup = TProc<TArgNode>;
  TArgNode = class(TParserNode)
  private
    FShortName: string;
    FArgType: PTypeInfo;

    FDefault: TValue;
    FRegEx: string;

    FValue: TValue;
    FHelp: String;
    function GetTypeRegEx(AType: PTypeInfo): string;
    function GetShortName: string;
    function ReadFromRawText(ArgType: PTypeInfo; RawText: string; var Value: TValue): Boolean;
  protected
    function DoConsume(const Parser: ICmdLnParser): Boolean; override;
  public
    constructor Create(const ArgType: PTypeInfo; const Names: string; Setup: TArgSetup=nil);
{$REGION 'Fluent API'}
    function Default(const Value: TValue): TArgNode;overload;
    function Default: TValue;overload;
    function RegEx(const Value: string): TArgNode;overload;
    function RegEx: string;overload;
    function Help: string;overload;
    function Help(const Value: string): TArgNode;overload;
{$ENDREGION}
    function IsType<T>: Boolean;
    property ShortName: string read GetShortName;
    property ArgType: PTypeInfo read FArgType;
    property Value: TValue read FValue write FValue;
  end;

  TCmdNode = class(TParserNode)
  private
    FArgs: TRepeatableNode;
    FCommands: TRepeatableNode;
  protected
    procedure ValidateShortName(const Arg: TArgNode);
    function DoConsume(const Parser: ICmdLnParser): Boolean; override;
    procedure BeforeAdd(const Node: TParserNode);override;
    procedure AfterAdd(const Node: TParserNode);override;
    procedure DoAddChild(const Node: TParserNode);override;
  public
    constructor Create(const Name: string; Parent: TParserNode = nil);
  end;

  TContextStack = class(TObjectStack<TCmdNode>)
  public
    function IsEmpty: Boolean;
  end;

  TCmdLnParserBuilder = class
  private
    FContextStack: TContextStack;
    function Context: TCmdNode;
    procedure Validate;
  public
    constructor Create;
    destructor Destroy;override;
    function Arg<T>(const ArgName: string; Setup: TArgSetup=nil): TCmdLnParserBuilder;overload;
    function Arg(const PInfo: PTypeInfo; ArgName: string; Setup: TArgSetup=nil): TCmdLnParserBuilder;overload;
    function Command(const CmdName: string): TCmdLnParserBuilder;
    function EndCommand: TCmdLnParserBuilder;
    function Build(AndFree: Boolean = True): ICmdLnParser;
  end;

implementation

uses
  System.StrUtils,
  System.Variants,
  Daf.Types,
  Daf.Arrays;

procedure DebugMsg(const Text: string);
begin
{$IFOPT D+}
  Debugger.Write(Text);
{$ENDIF}
end;

type
  TCmdLnParamsChildren = class(TDictionary<string, ICmdLParams>)
  public
    function IsEmpty: Boolean;
  end;

  TCmdLnParams = class(TInterfacedObject, ICmdLParams)
  private
    [Weak] FParent: ICmdLParams;
    FKey: string;
    FValue: TValue;
    FChildren: TCmdLnParamsChildren;
    function GetKey: string;
    function GetItem(Key: string): TValue;
    procedure SetItem(Key: string; const Value: TValue);
    function GetValue: TValue;
    procedure SetValue(const Value: TValue);
    function GetParent: ICmdLParams;
    function GetCommand: string;
  protected
    function GetPathValue(Segments: TArray<string>): TValue;
  public
    constructor Create(const Key: string = ''; const Parent: ICmdLParams = nil);
    destructor Destroy; override;
    function AddSection(const Key: string): TCmdLnParams;
    function HasItem(Key: string): Boolean;
    function HasSections: Boolean;
    function GetSection(Key: string): ICmdLParams;
    function GetSections: TEnumerable<ICmdLParams>;
    property Parent: ICmdLParams read GetParent;
    property Command: string read GetCommand;
    property Key: string read GetKey;
    property Value: TValue read GetValue write SetValue;
    property Item[Key: string]: TValue read GetItem write SetItem;
  end;

  TCmdLnParser = class(TInterfacedObject, ICmdLnParser)
  private
    FCmdLn: TArray<string>;
    FIndex: Integer;
    FRoot: TParserNode;
    FOptions: TCmdLnParserOpts;
    FContextParams: ICmdLParams;
    FRootParams: ICmdLParams;
    function GetCurrent: string;
    function GetOptions: TCmdLnParserOpts;
    function GetRoot: TParserNode;
  protected
    procedure BeginConsume(const Node: TParserNode);
    procedure EndConsume(const Node: TParserNode; const Ok: Boolean);
    procedure BeginConsumeCmd(const CmdNode: TCmdNode);
    procedure EndConsumeCmd(const CmdNode: TCmdNode; const Ok: Boolean);
    procedure BeginConsumeArg(const ArgNode: TArgNode);
    procedure EndConsumeArg(const ArgNode: TArgNode; const Ok: Boolean);
    function ValidateParse: Boolean;
  public
    constructor Create(Root: TParserNode);
    destructor Destroy; override;

    property Options: TCmdLnParserOpts read FOptions;
    property Root: TParserNode read GetRoot;

    property Current: string read GetCurrent;
    function MoveNext: Boolean;
    function InBounds: Boolean;

    function Consume(const Nodes: TParserNodes): Boolean;overload;
    function Consume(const Node: TParserNode): Boolean;overload;
    function Consume(const Pattern: string): Boolean;overload;
    function Consume(const Pattern: string; out Value: string): Boolean;overload;
    function Parse(const CmdLine: string): ICmdLParams;
  end;


{ TParserNode }

class function TParserNode.Error(FmtStr: string; Args: array of const): CmdLnParserError;
begin
  Result := CmdLnParserError.CreateFmt(FmtStr, Args);
end;

class function TParserNode.Error(Msg: string): CmdLnParserError;
begin
  Result := Error(Msg, []);
end;

constructor TParserNode.Create(const Name: string = ''; const Parent: TParserNode = nil);
begin
  inherited Create;
  FName := Name;
  FChildren := TParserNodes.Create(False); //Allow remove Nodes without destroy
  if Assigned(Parent) then
    Parent.AddChild(Self);
end;

destructor TParserNode.Destroy;
begin
  DebugMsg(Format('%s.Destroy; Name:%s;', [ClassName, Name]));
  FChildren.OwnsObjects := True;
  FChildren.Free;
  inherited;
end;

procedure TParserNode.SetParent(const Value: TParserNode);
begin
  if FParent = Value then Exit;
  FParent := Value;
end;

procedure TParserNode.BeforeAdd(const Node: TParserNode);
begin

end;

procedure TParserNode.DoAddChild(const Node: TParserNode);
begin
  FChildren.Add(Node);
end;

procedure TParserNode.AfterAdd(const Node: TParserNode);
begin

end;

function TParserNode.AddChild(const Node: TParserNode): TParserNode;
begin
  Result := Self;
  if Node.Parent = Self then Exit;

  BeforeAdd(Node);
  if not Node.IsRoot then
    Node.Parent.RemoveChild(Node);
  Node.SetParent(Self);
  DoAddChild(Node);
  AfterAdd(Node);
end;


procedure TParserNode.BeforeRemove(const Node: TParserNode);
begin

end;

procedure TParserNode.DoRemoveChild(const Node: TParserNode);
begin
  FChildren.Extract(Node);
end;

procedure TParserNode.AfterRemove(const Node: TParserNode);
begin

end;

function TParserNode.RemoveChild(const Node: TParserNode): TParserNode;
begin
  Result := Self;
  BeforeRemove(Node);
  DoRemoveChild(Node);
  AfterRemove(Node);
end;

function TParserNode.Required: Boolean;
begin
  Result := FRequired;
end;

function TParserNode.Required(const Value: Boolean): TParserNode;
begin
  FRequired := Value;
  Result := Self;
end;


function TParserNode.GetChild(Name: string): TParserNode;
begin
  for var Child in FChildren do
    if SameText(Child.Name, Name) then Exit(Child);
  Result := nil;
end;

function TParserNode.GetIsRoot: Boolean;
begin
  Result := (FParent = nil);
end;

function TParserNode.GetName: string;
begin
  Result := FName;
end;

{ TRepeatableNode }

constructor TRepeatableNode.Create(const Name: string = ''; const Parent: TParserNode = nil);
begin
  inherited Create(Name, Parent);
  FConsumedNodes := TParserNodes.Create(False);
  AddValidation(RequiredsAreConsumed);
end;

destructor TRepeatableNode.Destroy;
begin
  FConsumedNodes.Free;
  inherited;
end;

procedure TRepeatableNode.AddValidation(const Validator: TValidator);
begin
  var Aux: TArray<TValidator> := [TValidator((@Validator)^)];
  FValidations := TArray.Concat<TValidator>([FValidations, Aux]);
end;

function TRepeatableNode.ConsumedIsValid: Boolean;
begin
  for var Validator in FValidations do
    if not Validator(Self) then Exit(False);
  Result := True;
end;

class function TRepeatableNode.ConsumedIsNotEmpty(Validated: TRepeatableNode): Boolean;
begin
  Result := (Validated.FConsumedNodes.Count > 0) or (Validated.Children.Count = 0);
end;

class function TRepeatableNode.RequiredsAreConsumed(Validated: TRepeatableNode): Boolean;
begin
  for var Node in Validated.Children do
    if Node.Required and not Validated.FConsumedNodes.Contains(Node) then Exit(False);
  Result := True;
end;

function TRepeatableNode.ConsumeSomeNode(const Parser: ICmdLnParser): Boolean;
begin
  for var Node in Children do
    if Parser.Consume(Node) then
    begin
      FConsumedNodes.Add(Node);
      Exit(True);
    end;
  Result := False;
end;

function TRepeatableNode.DoConsume(const Parser: ICmdLnParser): Boolean;
begin
  FConsumedNodes.Clear;
  while Parser.InBounds and ConsumeSomeNode(Parser) do;
  Result := ConsumedIsValid;
end;

{ TArgNode }

constructor TArgNode.Create(const ArgType: PTypeInfo; const Names: string; Setup: TArgSetup=nil);
begin
  var NameList := Names.Split(['|']);
  inherited Create(NameList[0]);

  if Length(NameList) > 1 then
    FShortName := NameList[1];
  FArgType := ArgType;
  // RegEx to parse ArgType values
  FRegEx := GetTypeRegEx(ArgType);

  if Assigned(Setup) then
    Setup(Self);
end;

// P points a length field of ShortString.
function AfterString(const P: PByte): Pointer; inline;
begin
  Result := P + P^ + 1;
end;

function TArgNode.GetTypeRegEx(AType: PTypeInfo): string;
begin
  case AType.Kind of
    tkInteger, tkInt64:
      Result := '\d+';
    tkEnumeration:
      begin
        var
          T: PTypeData := GetTypeData(GetTypeData(ArgType)^.BaseType^);
        var
          P: Pointer := @T^.NameList;
        var
          Values: TArray<string>;
        SetLength(Values, 1 + T.MaxValue);
        for var idx := 0 to T.MaxValue do
        begin
          Values[idx] := UTF8IdentToString(PShortString(P));
          P := AfterString(P);
        end;
        Result := String.Join('|', Values);
      end;
    tkFloat:
      Result := '\d+(\' + FormatSettings.DecimalSeparator + '\d+)?';
    tkString, tkLString, tkWString, tkUString:
      Result := '.+';
    tkChar, tkWChar:
      Result := '.';
    tkSet, tkArray, tkDynArray:
      Result := '(\s*\w+\s*(,\s*\w+\s*)*)';
  end;
end;

function TArgNode.ReadFromRawText(ArgType: PTypeInfo; RawText: string; var Value: TValue): Boolean;
begin
  // Ensure Value is of ArgType
  TValue.Make(nil, ArgType, Value);
  case ArgType.Kind of
    tkEnumeration: TValue.Make(IndexText(RawText, RegEx.Split(['|'])), ArgType, FValue);
    tkArray, tkDynArray:
    begin
     var
      RawItems := TArray.Trim(RawText.Split([',']));
      var
        ItemType: PTypeInfo;
        if ArgType.Kind = tkDynArray  then
          ItemType := ArgType.TypeData.DynArrElType^
        else
          ItemType := ArgType.TypeData.ArrayData.ElType^;
      var
        Items := TArray.Map<string, TValue>(RawItems,
          function(RawItem: string): TValue
          begin
            ReadFromRawText(ItemType, RawItem, Result);
          end
          );
      Value := TValue.FromArray(ArgType, Items);
    end
    else
    begin
      var Src: Variant := RawText;
      var Dest: Variant := Value.AsVariant;
      VarCast(Dest, Src, VarType(Dest));
      Value := TValue.FromVariant(Dest);
    end;
  end;
  Result := True;
end;

function TArgNode.GetShortName: string;
begin
    Result := FShortName;
end;

function TArgNode.IsType<T>: Boolean;
begin
  Result := (FArgType = TypeInfo(T));
end;

{$REGION 'Fluent API'}
function TArgNode.RegEx: string;
begin
  Result := FRegEx;
end;

function TArgNode.RegEx(const Value: string): TArgNode;
begin
  FRegEx := Value;
  Result := Self;
end;

function TArgNode.Default: TValue;
begin
  Result := FDefault;
end;

function TArgNode.Default(const Value: TValue): TArgNode;
begin
  FDefault := Value;
  Result := Self;
end;

function TArgNode.Help: string;
begin
  Result := FHelp;
end;

function TArgNode.Help(const Value: string): TArgNode;
begin
  FHelp := Value;
  Result := Self;
end;
{$ENDREGION}

function TArgNode.DoConsume(const Parser: ICmdLnParser): Boolean;
begin
 with Parser.Options do
  Result := Parser.Consume(LongArgPrefix + Name) or (not ShortName.IsEmpty and Parser.Consume(ShortArgPrefix + ShortName));
  if not Result then
    Exit;

  var
    RawText: string;
  Result := Parser.Consume(RegEx, RawText);

  if not Result and IsType<Boolean> then
  begin
    RawText := True.ToString(TUseBoolStrs.True);
    Result := True;
  end;

  if not Result then
    Exit;
  Result := ReadFromRawText(ArgType, RawText, FValue);
end;

{ TCmdNode }

constructor TCmdNode.Create(const Name: string; Parent: TParserNode);
begin
  inherited Create(Name, Parent);
  FArgs := TRepeatableNode.Create('Args', Self);
  FCommands := TRepeatableNode.Create('Cmds', Self);
  FCommands.AddValidation(TRepeatableNode.ConsumedIsNotEmpty);
end;

procedure TCmdNode.ValidateShortName(const Arg: TArgNode);
begin
  if Arg.ShortName.IsEmpty then Exit;
  for var Other in FArgs.Children do
  begin
    if SameText(Arg.ShortName, (Other as TArgNode).ShortName) then
      raise Error('Arg %s duplicates short name %s', [Arg.Name, Arg.ShortName]);
  end;
end;

procedure TCmdNode.BeforeAdd(const Node: TParserNode);
begin
  if Node is TArgNode then
    ValidateShortName(TArgNode(Node));
end;

procedure TCmdNode.AfterAdd(const Node: TParserNode);
begin

end;

procedure TCmdNode.DoAddChild(const Node: TParserNode);
begin
  if Node is TArgNode then
    FArgs.AddChild(Node)
  else if Node is TCmdNode then
    FCommands.AddChild(Node)
  else
    inherited;
end;

function TCmdNode.DoConsume(const Parser: ICmdLnParser): Boolean;
begin
  Result := Parser.Consume(Name)
        and Parser.Consume(FArgs)
        and Parser.Consume(FCommands);
end;

{ TCmdLnParserOpts }

class operator TCmdLnParserOpts.Initialize(out Dest: TCmdLnParserOpts);
begin
  Dest.QuoteChar := '"';
  Dest.LongArgPrefix := '--';
  Dest.ShortArgPrefix := '-';
end;

{ TCmdLnParser }

constructor TCmdLnParser.Create(Root: TParserNode);
begin
  inherited Create;
  FRoot := Root;
  FRootParams := TCmdLnParams.Create;
  FContextParams := FRootParams;
end;

destructor TCmdLnParser.Destroy;
begin
  FContextParams := nil;
  FRoot.Free;
  inherited;
end;

function TCmdLnParser.GetCurrent: string;
begin
  Result := FCmdLn[FIndex];
end;

function TCmdLnParser.GetOptions: TCmdLnParserOpts;
begin
  Result := FOptions;
end;

function TCmdLnParser.GetRoot: TParserNode;
begin
  Result := FRoot;
end;

function TCmdLnParser.InBounds: Boolean;
begin
  Result := (FIndex >= 0) and (FIndex < Length(FCmdLn));
end;

function TCmdLnParser.MoveNext: Boolean;
begin
  Inc(FIndex);
  Result := InBounds;
end;

procedure TCmdLnParser.BeginConsumeArg(const ArgNode: TArgNode);
begin

end;

procedure TCmdLnParser.EndConsumeArg(const ArgNode: TArgNode; const Ok: Boolean);
begin
  if OK then
    FContextParams.GetSection(ArgNode.Name).Value := ArgNode.Value;
end;

procedure TCmdLnParser.BeginConsumeCmd(const CmdNode: TCmdNode);
begin
  if not CmdNode.IsRoot then
    FContextParams := FContextParams.GetSection(CmdNode.Name);
  if not Assigned(FContextParams) then
    Assert(Assigned(FContextParams))
end;

procedure TCmdLnParser.EndConsumeCmd(const CmdNode: TCmdNode; const Ok: Boolean);
begin
  if not CmdNode.IsRoot then
    FContextParams := FContextParams.Parent;
  if not Ok then
    FContextParams.Value := TValue.Empty;
end;

procedure TCmdLnParser.BeginConsume(const Node: TParserNode);
begin
  if Node is TCmdNode then
    BeginConsumeCmd(TCmdNode(Node))
  else if Node is TArgNode then
    BeginConsumeArg(TArgNode(Node));
end;

procedure TCmdLnParser.EndConsume(const Node: TParserNode; const Ok: Boolean);
begin
  if Node is TCmdNode then
    EndConsumeCmd(TCmdNode(Node), Ok)
  else if Node is TArgNode then
    EndConsumeArg(TArgNode(Node), Ok);
end;

function TCmdLnParser.Consume(const Pattern: string): Boolean;
begin
  var  dummy: string;
  Result := Consume(Pattern, dummy);
end;

function TCmdLnParser.Consume(const Pattern: string; out Value: string): Boolean;
begin
  Value := '';
  if Pattern.IsEmpty then Exit(True);
  if not InBounds then Exit(False);

  var
    Input := Current.DeQuotedString(Options.QuoteChar);
  var
    RgEx := TRegEx.Create(Format('^%0:s', [Pattern]), [roIgnoreCase]);

  if not RgEx.IsMatch(Input) then Exit(False);

  Value := RgEx.Match(Input).Value;
  MoveNext;
  Result := True;
end;

function TCmdLnParser.Consume(const Node: TParserNode): Boolean;
begin
  BeginConsume(Node);
  Result := Node.DoConsume(Self);
  EndConsume(Node, Result);
end;

function TCmdLnParser.Consume(const Nodes: TParserNodes): Boolean;
begin
  for var Node in Nodes do
    if Consume(Node)then
      Exit(True);
  Result := not InBounds;
end;

function TCmdLnParser.Parse(const CmdLine: string): ICmdLParams;
begin
  FCmdLn := TArray.Trim(CmdLine.Split([' '], Options.QuoteChar));
  FIndex := 0;
  if Consume(FRoot) and ValidateParse then
    Result :=  FRootParams
  else
    Result := nil;
end;

function TCmdLnParser.ValidateParse: Boolean;
begin
  Result := FContextParams = FRootParams;
end;

{ TCmdLnParserBuilder }

constructor TCmdLnParserBuilder.Create;
begin
  inherited;
  FContextStack := TContextStack.Create(False);
end;

destructor TCmdLnParserBuilder.Destroy;
begin
  FreeAndNil(FContextStack);
  inherited;
end;

function TCmdLnParserBuilder.Arg<T>(const ArgName: string; Setup: TArgSetup=nil): TCmdLnParserBuilder;
begin
  Result := Arg(TypeInfo(T), ArgName, Setup)
end;

function TCmdLnParserBuilder.Arg(const PInfo: PTypeInfo;  ArgName: string; Setup: TArgSetup=nil): TCmdLnParserBuilder;
begin
  Result := Self;
  Context.AddChild(TArgNode.Create(PInfo, ArgName, Setup));
end;

function TCmdLnParserBuilder.Command(const CmdName: string): TCmdLnParserBuilder;
begin
  Result := Self;
  FContextStack.Push(TCmdNode.Create(CmdName, Context));
end;

function TCmdLnParserBuilder.EndCommand: TCmdLnParserBuilder;
begin
  Result := Self;
  FContextStack.Pop;
end;

function TCmdLnParserBuilder.Build(AndFree: Boolean = True): ICmdLnParser;
begin
  Validate;
  Result := TCmdLnParser.Create(Context);
  if AndFree then
    Free;
end;

function TCmdLnParserBuilder.Context: TCmdNode;
begin
  if FContextStack.IsEmpty then
    FContextStack.Push(TCmdNode.Create('', nil));
  Result := FContextStack.Peek;
end;

procedure TCmdLnParserBuilder.Validate;
begin
  if not Context.IsRoot then
    raise TParserNode.Error('Command %s not completed', [Context.Name]);
end;

{ TCmdLnParams }

constructor TCmdLnParams.Create(const Key: string = ''; const Parent: ICmdLParams = nil);
begin
  inherited Create;
  FParent := Parent;
  FKey := Key;
  FChildren := TCmdLnParamsChildren.Create;
end;

destructor TCmdLnParams.Destroy;
begin
  FChildren.Free;
  inherited;
end;

function TCmdLnParams.GetSections: TEnumerable<ICmdLParams>;
begin
  Result := FChildren.Values;
end;

function TCmdLnParams.GetValue: TValue;
begin
  Result := FValue;
end;

procedure TCmdLnParams.SetValue(const Value: TValue);
begin
  FValue := Value;
  FChildren.Clear;
end;

function TCmdLnParams.AddSection(const Key: string): TCmdLnParams;
begin
  Result := TCmdLnParams.Create(Key, Self);
  FChildren.Add(Key, Result);
  FValue := TValue.Empty;
end;

function TCmdLnParams.GetSection(Key: string): ICmdLParams;
begin
  var Opts: ICmdLParams;
  if not FChildren.TryGetValue(Key, Opts) then
    Opts := AddSection(Key);
  Result := Opts;
end;

procedure TCmdLnParams.SetItem(Key: string; const Value: TValue);
begin
  GetSection(Key).Value := Value;
end;

function TCmdLnParams.GetParent: ICmdLParams;
begin
  Result := FParent;
end;

function TCmdLnParams.GetPathValue(Segments: TArray<string>): TValue;
begin
  var Len := Length(Segments);
  case Len of
     0: Result := TValue.Empty;
     1: Result := GetSection(Segments[0]).Value;
     else
     begin
       var Section := TCmdLnParams(GetSection(Segments[0]));
       var NewSegments: TArray<string>;
       SetLength(NewSegments, Len - 1);
       TArray.Copy<string>(Segments, NewSegments, 1, 0, Len - 1);
       Result := Section.GetPathValue(NewSegments);
     end;
  end;
end;

function TCmdLnParams.GetCommand: string;
begin
  for var Sec in GetSections do
  begin
    if not HasItem(Sec.Key) then
      Exit(Sec.Key);
  end;
  Result := '';
end;

function TCmdLnParams.GetItem(Key: string): TValue;
begin
  var segments := Key.Split(['.']);
  Result := GetPathValue(segments);
end;

function TCmdLnParams.GetKey: string;
begin
  Result := FKey;
end;

function TCmdLnParams.HasItem(Key: string): Boolean;
begin
  Result := FChildren.ContainsKey(Key) and not FChildren[Key].HasSections;
end;

function TCmdLnParams.HasSections: Boolean;
begin
  Result := not FChildren.IsEmpty;
end;

{ TContextStack }

function TContextStack.IsEmpty: Boolean;
begin
  Result := Self.Count = 0;
end;

{ TCmdLnParamsChildren }

function TCmdLnParamsChildren.IsEmpty: Boolean;
begin
  Result := Count = 0;
end;

end.
