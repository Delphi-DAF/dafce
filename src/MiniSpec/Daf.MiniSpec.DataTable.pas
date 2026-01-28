unit Daf.MiniSpec.DataTable;

interface

uses
  System.Rtti,
  System.SysUtils,
  System.Generics.Collections;

type
  /// <summary>
  /// Raw DataTable type: 2D array of TValue.
  /// First row is typically headers.
  /// </summary>
  TDataTable = TArray<TArray<TValue>>;

  /// <summary>
  /// Wrapper class providing convenient access methods for DataTable data.
  /// </summary>
  TDataTableObj = class
  strict private
    FData: TDataTable;
    FHeaders: TArray<string>;
    function GetHeaders: TArray<string>;
    function GetRowCount: Integer;
    function GetColCount: Integer;
  public
    constructor Create(const AData: TDataTable);

    /// <summary>
    /// Returns data rows (excluding header row).
    /// </summary>
    function Rows: TArray<TArray<TValue>>;

    /// <summary>
    /// Returns raw data including headers.
    /// </summary>
    function Raw: TDataTable;

    /// <summary>
    /// Returns number of data rows (excluding headers).
    /// </summary>
    property RowCount: Integer read GetRowCount;

    /// <summary>
    /// Returns number of columns.
    /// </summary>
    property ColCount: Integer read GetColCount;

    /// <summary>
    /// Returns header names from first row.
    /// </summary>
    property Headers: TArray<string> read GetHeaders;

    /// <summary>
    /// Gets cell value by row and column index (0-based, row excludes header).
    /// </summary>
    function Cell(Row, Col: Integer): TValue; overload;

    /// <summary>
    /// Gets cell value by row index and column name.
    /// </summary>
    function Cell(Row: Integer; const ColName: string): TValue; overload;

    /// <summary>
    /// Returns a single row as dictionary (column name -> value).
    /// RowIndex is 0-based (excludes header).
    /// </summary>
    function AsMap(RowIndex: Integer): TDictionary<string, TValue>;

    /// <summary>
    /// Returns all data rows as array of dictionaries.
    /// </summary>
    function AsList: TArray<TDictionary<string, TValue>>;

    /// <summary>
    /// Returns a new DataTableObj with rows and columns transposed.
    /// </summary>
    function Transpose: TDataTableObj;
  end;

implementation

{ TDataTableObj }

constructor TDataTableObj.Create(const AData: TDataTable);
begin
  inherited Create;
  FData := AData;
  // Extract headers from first row
  if Length(AData) > 0 then
  begin
    SetLength(FHeaders, Length(AData[0]));
    for var i := 0 to High(AData[0]) do
      FHeaders[i] := AData[0][i].AsString;
  end;
end;

function TDataTableObj.GetHeaders: TArray<string>;
begin
  Result := FHeaders;
end;

function TDataTableObj.GetRowCount: Integer;
begin
  if Length(FData) > 1 then
    Result := Length(FData) - 1  // Exclude header row
  else
    Result := 0;
end;

function TDataTableObj.GetColCount: Integer;
begin
  if Length(FData) > 0 then
    Result := Length(FData[0])
  else
    Result := 0;
end;

function TDataTableObj.Rows: TArray<TArray<TValue>>;
begin
  if Length(FData) > 1 then
  begin
    SetLength(Result, Length(FData) - 1);
    for var i := 1 to High(FData) do
      Result[i - 1] := FData[i];
  end
  else
    SetLength(Result, 0);
end;

function TDataTableObj.Raw: TDataTable;
begin
  Result := FData;
end;

function TDataTableObj.Cell(Row, Col: Integer): TValue;
begin
  // Row is 0-based for data rows (header is excluded)
  if (Row < 0) or (Row >= RowCount) then
    raise EArgumentOutOfRangeException.CreateFmt('Row index %d out of range (0..%d)', [Row, RowCount - 1]);
  if (Col < 0) or (Col >= ColCount) then
    raise EArgumentOutOfRangeException.CreateFmt('Column index %d out of range (0..%d)', [Col, ColCount - 1]);
  Result := FData[Row + 1][Col];  // +1 to skip header
end;

function TDataTableObj.Cell(Row: Integer; const ColName: string): TValue;
var
  ColIndex: Integer;
begin
  ColIndex := -1;
  for var i := 0 to High(FHeaders) do
  begin
    if SameText(FHeaders[i], ColName) then
    begin
      ColIndex := i;
      Break;
    end;
  end;
  if ColIndex < 0 then
    raise EArgumentException.CreateFmt('Column "%s" not found', [ColName]);
  Result := Cell(Row, ColIndex);
end;

function TDataTableObj.AsMap(RowIndex: Integer): TDictionary<string, TValue>;
begin
  Result := TDictionary<string, TValue>.Create;
  for var i := 0 to High(FHeaders) do
    Result.Add(FHeaders[i], Cell(RowIndex, i));
end;

function TDataTableObj.AsList: TArray<TDictionary<string, TValue>>;
begin
  SetLength(Result, RowCount);
  for var i := 0 to RowCount - 1 do
    Result[i] := AsMap(i);
end;

function TDataTableObj.Transpose: TDataTableObj;
var
  Transposed: TDataTable;
begin
  if Length(FData) = 0 then
    Exit(TDataTableObj.Create(nil));

  SetLength(Transposed, ColCount, Length(FData));
  for var Row := 0 to High(FData) do
    for var Col := 0 to High(FData[Row]) do
      Transposed[Col][Row] := FData[Row][Col];

  Result := TDataTableObj.Create(Transposed);
end;

end.
