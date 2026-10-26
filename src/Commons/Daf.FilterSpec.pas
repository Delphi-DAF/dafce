unit Daf.FilterSpec;

{
  TFilterSpec<T> — the closure world's predicate: a filter you compose with
  and/or/not and evaluate over data you already hold in memory.

  It is the counterpart of, and deliberately not the same thing as, the tree
  world of Daf.Specification (IExpr / TSpecification<T>), whose criteria are
  inspectable and can be translated to a back-end. A closure cannot be
  translated, so the two never share a unit and never share a name.
}

interface

uses
  System.SysUtils;

type
  TFilterSpec<T> = record
  private
    FClosureExpr: TPredicate<T>;
  public
    class operator Implicit(FilterExpr: Boolean): TFilterSpec<T>;
    class operator Implicit(FilterExpr: TPredicate<T>): TFilterSpec<T>;
    class operator Implicit(FilterExpr: TFilterSpec<T>): TPredicate<T>;
    class operator LogicalNot(Right: TFilterSpec<T>): TFilterSpec<T>;
    class operator LogicalAnd(Left: TFilterSpec<T>; Right: TFilterSpec<T>): TFilterSpec<T>;
    class operator LogicalOr(Left: TFilterSpec<T>; Right: TFilterSpec<T>): TFilterSpec<T>;
    function Eval(const Item: T): Boolean;
  end;

implementation

{ TFilterSpec<T> }

function TFilterSpec<T>.Eval(const Item: T): Boolean;
begin
  Result := not Assigned(FClosureExpr) or FClosureExpr(Item);
end;

class operator TFilterSpec<T>.Implicit(FilterExpr: TPredicate<T>): TFilterSpec<T>;
begin
  Result.FClosureExpr := FilterExpr;
end;

class operator TFilterSpec<T>.Implicit(FilterExpr: Boolean): TFilterSpec<T>;
begin
  Result.FClosureExpr := function(Item: T): Boolean
    begin
      Result := FilterExpr;
    end;
end;

class operator TFilterSpec<T>.LogicalNot(Right: TFilterSpec<T>): TFilterSpec<T>;
begin
  Result.FClosureExpr := function(Item: T): Boolean
    begin
      Result := not Right.Eval(Item);
    end;
end;

class operator TFilterSpec<T>.LogicalAnd(Left: TFilterSpec<T>; Right: TFilterSpec<T>): TFilterSpec<T>;
begin
  Result.FClosureExpr := function(Item: T): Boolean
    begin
      Result := Left.Eval(Item) And Right.Eval(Item);
    end;
end;

class operator TFilterSpec<T>.LogicalOr(Left: TFilterSpec<T>; Right: TFilterSpec<T>): TFilterSpec<T>;
begin
  Result.FClosureExpr := function(Item: T): Boolean
    begin
      Result := Left.Eval(Item) or Right.Eval(Item);
    end;
end;

class operator TFilterSpec<T>.Implicit(FilterExpr: TFilterSpec<T>): TPredicate<T>;
begin
  Result := FilterExpr.FClosureExpr;
end;

end.
