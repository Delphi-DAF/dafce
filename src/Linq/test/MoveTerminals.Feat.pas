unit MoveTerminals.Feat;

interface

uses
  System.Generics.Collections,
  Daf.Linq,
  Ownership.Doubles;

type
  TMoveWorld = class
  public
    Source: TTrackedList;
    Target: TTrackedList;
    A, B, C: TTracked;
    Moved: TArray<TTracked>;
    MovedList: TList<TTracked>;
    Raised: string;
    destructor Destroy; override;
  end;

implementation

uses
  System.SysUtils,
  System.Generics.Defaults,
  Daf.MiniSpec;

{ TMoveWorld }

destructor TMoveWorld.Destroy;
begin
  MovedList.Free;
  Tracking.FreeSurvivors;
  inherited;
end;

procedure FillABC(W: TMoveWorld);
begin
  W.Source := TTrackedList.Create('Source');
  W.A := TTracked.Create('A');
  W.B := TTracked.Create('B');
  W.C := TTracked.Create('C');
  W.Source.AddRange([W.A, W.B, W.C]);
end;

// Reads the names of the elements, so it is only called on a list whose elements are alive.
function NamesIn(const Items: array of TTracked): string;
begin
  Result := '';
  for var X in Items do
  begin
    if Result <> '' then
      Result := Result + ',';
    Result := Result + X.Name;
  end;
end;

function NotB: TPredicate<TTracked>;
begin
  Result :=
    function(X: TTracked): Boolean
    begin
      Result := X.Name <> 'B';
    end;
end;

initialization

Feature('''
Feature Terminals that move elements out of the source list @linq @move

  As a developer who keeps a list and wants part of it in another
  I want a terminal that takes the selected elements out of the source as it delivers them
  So that each object keeps a single owner without a separate loop of Extract calls
''')

.UseWorld<TMoveWorld>

.Rule('MoveToList and MoveToArray take what they deliver out of the source list')

  .Scenario('Partitioning a list')
    .Given('a source list holding A, B and C, and an empty target',
      procedure(W: TMoveWorld)
      begin
        FillABC(W);
        W.Target := TTrackedList.Create('Target');
        Expect(NamesIn(W.Source.ToArray)).ToEqual('A,B,C');
      end)
    .When('MoveToList(Target) is called on the query filtered to A and C',
      procedure(W: TMoveWorld)
      begin
        Linq.From<TTracked>(W.Source).Where(NotB()).MoveToList(W.Target);
      end)
    .&Then('Target holds A and C in that order, the source holds only B, and nothing was freed',
      procedure(W: TMoveWorld)
      begin
        Expect(NamesIn(W.Target.ToArray)).ToEqual('A,C');
        Expect(NamesIn(W.Source.ToArray)).ToEqual('B');
        Expect(Tracking.AliveNames).ToEqual('Source,A,B,C,Target');
      end)

  .Scenario('Consecutive selected elements are all moved')
    .Given('a source list holding A, B and C, and an empty target',
      procedure(W: TMoveWorld)
      begin
        FillABC(W);
        W.Target := TTrackedList.Create('Target');
        Expect(NamesIn(W.Source.ToArray)).ToEqual('A,B,C');
      end)
    .When('MoveToList(Target) is called on a query that selects every element',
      procedure(W: TMoveWorld)
      begin
        Linq.From<TTracked>(W.Source).MoveToList(W.Target);
      end)
    .&Then('the source is empty and Target holds A, B and C',
      procedure(W: TMoveWorld)
      begin
        Expect(W.Source.Count).ToEqual(0);
        Expect(NamesIn(W.Target.ToArray)).ToEqual('A,B,C');
      end)

  .Scenario('Into a new list')
    .Given('a source list holding A, B and C',
      procedure(W: TMoveWorld)
      begin
        FillABC(W);
        Expect(NamesIn(W.Source.ToArray)).ToEqual('A,B,C');
      end)
    .When('MoveToList without arguments is called with a filter that selects A and C',
      procedure(W: TMoveWorld)
      begin
        W.MovedList := Linq.From<TTracked>(W.Source).Where(NotB()).MoveToList;
      end)
    .&Then('the new list holds A and C, and the source holds only B',
      procedure(W: TMoveWorld)
      begin
        Expect(NamesIn(W.MovedList.ToArray)).ToEqual('A,C');
        Expect(NamesIn(W.Source.ToArray)).ToEqual('B');
      end)

  .Scenario('Into a new array')
    .Given('a source list holding A, B and C',
      procedure(W: TMoveWorld)
      begin
        FillABC(W);
        Expect(NamesIn(W.Source.ToArray)).ToEqual('A,B,C');
      end)
    .When('MoveToArray is called with a filter that selects A and C',
      procedure(W: TMoveWorld)
      begin
        W.Moved := Linq.From<TTracked>(W.Source).Where(NotB()).MoveToArray;
      end)
    .&Then('the array holds A and C, and the source holds only B',
      procedure(W: TMoveWorld)
      begin
        Expect(NamesIn(W.Moved)).ToEqual('A,C');
        Expect(NamesIn(W.Source.ToArray)).ToEqual('B');
      end)

  .Scenario('Through operators that keep the element type')
    .Given('a source list holding A, B and C, and an empty target',
      procedure(W: TMoveWorld)
      begin
        FillABC(W);
        W.Target := TTrackedList.Create('Target');
        Expect(NamesIn(W.Source.ToArray)).ToEqual('A,B,C');
      end)
    .When('MoveToList(Target) is called after Where and a descending OrderBy',
      procedure(W: TMoveWorld)
      begin
        Linq.From<TTracked>(W.Source).Where(NotB()).OrderBy(TComparer<TTracked>.Construct(
          function(const L, R: TTracked): Integer
          begin
            Result := CompareStr(R.Name, L.Name);
          end)).MoveToList(W.Target);
      end)
    .&Then('the selected elements are moved in the OrderBy order',
      procedure(W: TMoveWorld)
      begin
        Expect(NamesIn(W.Target.ToArray)).ToEqual('C,A');
        Expect(NamesIn(W.Source.ToArray)).ToEqual('B');
      end)

  .Scenario('Each object keeps one owner')
    .Given('a source and a target that both own their objects',
      procedure(W: TMoveWorld)
      begin
        FillABC(W);
        W.Target := TTrackedList.Create('Target');
        Expect(Tracking.AliveNames).ToEqual('Source,A,B,C,Target');
      end)
    .When('MoveToList(Target) moves A and C, and both lists are then freed',
      procedure(W: TMoveWorld)
      begin
        Linq.From<TTracked>(W.Source).Where(NotB()).MoveToList(W.Target);
        // Only when no object ended in both lists: otherwise freeing both would free it twice.
        var Shared := False;
        for var X in W.Target do
          if W.Source.IndexOf(X) >= 0 then
            Shared := True;
        if not Shared then
        begin
          W.Source.Free;
          W.Source := nil;
          W.Target.Free;
          W.Target := nil;
        end;
      end)
    .&Then('every object has been freed exactly once',
      procedure(W: TMoveWorld)
      begin
        Expect(Tracking.AliveNames).ToEqual('');
      end)

  .Scenario('Over an owned source')
    .Given('a source list holding A, B and C, and a target that owns its objects',
      procedure(W: TMoveWorld)
      begin
        FillABC(W);
        W.Target := TTrackedList.Create('Target');
        Expect(Tracking.AliveNames).ToEqual('Source,A,B,C,Target');
      end)
    .When('MoveToList(Target) is called on a query built with FromOwned, filtered to A and C',
      procedure(W: TMoveWorld)
      begin
        Linq.FromOwned<TTracked>(W.Source).Where(NotB()).MoveToList(W.Target);
      end)
    .&Then('once the query is released, A and C are alive in Target and the rest was freed',
      procedure(W: TMoveWorld)
      begin
        Expect(Tracking.AliveNames).ToEqual('A,C,Target');
        Expect(W.Target.Count).ToEqual(2);
      end)

.Rule('Moving needs a source list')

  .Scenario('An array source')
    .Given('three objects in an array, and an empty target',
      procedure(W: TMoveWorld)
      begin
        FillABC(W);
        W.Target := TTrackedList.Create('Target');
        Expect(W.Target.Count).ToEqual(0);
      end)
    .When('MoveToList(Target) is called on a query built from the array',
      procedure(W: TMoveWorld)
      begin
        var Items: TArray<TTracked> := [W.A, W.B, W.C];
        try
          Linq.From<TTracked>(Items).MoveToList(W.Target);
        except
          on E: ELinqError do
            W.Raised := E.ClassName;
        end;
      end)
    .&Then('ELinqError is raised and Target stays empty',
      procedure(W: TMoveWorld)
      begin
        Expect(W.Raised).ToEqual('ELinqError');
        Expect(W.Target.Count).ToEqual(0);
      end)

  .Scenario('A projected chain')
    .Given('a source list holding A, B and C, and an empty target',
      procedure(W: TMoveWorld)
      begin
        FillABC(W);
        W.Target := TTrackedList.Create('Target');
        Expect(NamesIn(W.Source.ToArray)).ToEqual('A,B,C');
      end)
    .When('MoveToList(Target) is called on a query that went through Select',
      procedure(W: TMoveWorld)
      begin
        try
          Linq.From<TTracked>(W.Source).Select<TTracked>(
            function(X: TTracked): TTracked
            begin
              Result := X;
            end).MoveToList(W.Target);
        except
          on E: ELinqError do
            W.Raised := E.ClassName;
        end;
      end)
    .&Then('ELinqError is raised, and the source keeps all its elements',
      procedure(W: TMoveWorld)
      begin
        Expect(W.Raised).ToEqual('ELinqError');
        Expect(NamesIn(W.Source.ToArray)).ToEqual('A,B,C');
        Expect(W.Target.Count).ToEqual(0);
      end)

.Rule('A target that raises while moving leaves one owner')

  .Scenario('The target keeps the element and then raises')
    .Given('a source holding A, B and C, and a target that raises on receiving B after keeping it',
      procedure(W: TMoveWorld)
      begin
        FillABC(W);
        W.Target := TRaisingList.Create('Target');
        TRaisingList(W.Target).Bad := W.B;
        Expect(NamesIn(W.Source.ToArray)).ToEqual('A,B,C');
      end)
    .When('MoveToList(Target) is called',
      procedure(W: TMoveWorld)
      begin
        try
          Linq.From<TTracked>(W.Source).MoveToList(W.Target);
        except
          on E: Exception do
            W.Raised := E.Message;
        end;
      end)
    .&Then('the exception reaches the caller, A and B are in the target, and C is still in the source',
      procedure(W: TMoveWorld)
      begin
        Expect(W.Raised).ToEqual('rechazo B');
        Expect(NamesIn(W.Target.ToArray)).ToEqual('A,B');
        Expect(NamesIn(W.Source.ToArray)).ToEqual('C');
      end);

end.
