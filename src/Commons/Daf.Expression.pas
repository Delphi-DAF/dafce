unit Daf.Expression;

interface
uses
  System.SysUtils;

type
  TExpresion<T> = record
  private
    FFunc: TFunc<T>;
  public
    class operator Initialize(out Dest: TExpresion<T>);
    class operator Implicit(const Value: T): TExpresion<T>;
    class operator Implicit(const Func: TFunc<T>): TExpresion<T>;
    class operator Implicit(const Expresion: TExpresion<T>): T;
  end;

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

{ TExpresion<T> }

class operator TExpresion<T>.Implicit(const Value: T): TExpresion<T>;
begin
  Result.FFunc := function: T
    begin
      Result := Value
    end;
end;

class operator TExpresion<T>.Implicit(const Func: TFunc<T>): TExpresion<T>;
begin
  Result.FFunc := Func;
end;

class operator TExpresion<T>.Implicit(const Expresion: TExpresion<T>): T;
begin
  Result := Expresion.FFunc();
end;

class operator TExpresion<T>.Initialize(out Dest: TExpresion<T>);
begin
  Dest.FFunc := function: T
    begin
      Result := Default(T);
    end;
end;

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
