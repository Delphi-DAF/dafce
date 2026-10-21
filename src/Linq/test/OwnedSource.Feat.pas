unit OwnedSource.Feat;

interface

uses
  Daf.Linq,
  Ownership.Doubles;

type
  TOwnedWorld = class
  public
    Source: TTrackedList;
    Other: TTrackedList;
    Target: TTrackedList;
    Second: TTrackedList;
    A, B, C: TTracked;
    Got, Got2: TTracked;
    Query: TQuery<TTracked>;
    Derived: TQuery<TTracked>;
    Strings: TArray<string>;
    Seen: string;
    AfterFirst: string;
    AfterDerived: string;
    Answers: string;
    Raised: string;
    destructor Destroy; override;
  end;

implementation

uses
  System.SysUtils,
  Daf.MiniSpec;

{ TOwnedWorld }

destructor TOwnedWorld.Destroy;
begin
  Query := Default(TQuery<TTracked>);
  Derived := Default(TQuery<TTracked>);
  Tracking.FreeSurvivors;
  inherited;
end;

procedure FillABC(W: TOwnedWorld);
begin
  W.Source := TTrackedList.Create('Lista');
  W.A := TTracked.Create('A');
  W.B := TTracked.Create('B');
  W.C := TTracked.Create('C');
  W.Source.AddRange([W.A, W.B, W.C]);
end;

procedure FillAB(W: TOwnedWorld);
begin
  W.Source := TTrackedList.Create('Lista');
  W.A := TTracked.Create('A');
  W.B := TTracked.Create('B');
  W.Source.AddRange([W.A, W.B]);
end;

function NamesOf(const Q: TQuery<TTracked>): string;
begin
  Result := '';
  for var X in Q do
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

// The Then steps never read an object that the code under test may have freed: they ask the
// registry, by name or by pointer, so a red scenario reports instead of crashing.
function AliveText(const Items: array of TTracked): string;
begin
  Result := '';
  for var X in Items do
  begin
    if Result <> '' then
      Result := Result + ',';
    Result := Result + BoolToStr(Tracking.IsAlive(X), True);
  end;
end;

initialization

Feature('''
Feature Una fuente que Linq posee @linq @owned

  Como quien consulta con Linq una lista que nadie mas guarda
  Quiero que Linq se quede con la lista y la libere cuando ya no se use
  Para no tener que guardarla en una variable solo para liberarla
''')

.UseWorld<TOwnedWorld>

.Rule('FromOwned se queda con la lista y la libera con la ultima consulta')

  .Scenario('Una lista que desciende de TObjectList')
    .Given('una lista propia con A, B y C',
      procedure(W: TOwnedWorld)
      begin
        FillABC(W);
        Expect(Tracking.AliveNames).ToEqual('Lista,A,B,C');
      end)
    .When('se recorre una consulta de FromOwned sobre ella',
      procedure(W: TOwnedWorld)
      begin
        W.Seen := NamesOf(Linq.FromOwned<TTracked>(W.Source));
      end)
    .&Then('produce sus elementos en el orden de la lista',
      procedure(W: TOwnedWorld)
      begin
        Expect(W.Seen).ToEqual('A,B,C');
      end)

  .Scenario('Una consulta derivada mantiene viva la fuente')
    .Given('una consulta de fuente poseida y otra derivada de ella con Where',
      procedure(W: TOwnedWorld)
      begin
        FillABC(W);
        W.Query := Linq.FromOwned<TTracked>(W.Source);
        W.Derived := W.Query.Where(NotB());
        Expect(Tracking.AliveNames).ToEqual('Lista,A,B,C');
      end)
    .When('se suelta la primera y despues la derivada',
      procedure(W: TOwnedWorld)
      begin
        W.Query := Default(TQuery<TTracked>);
        W.AfterFirst := Tracking.AliveNames;
        W.Derived := Default(TQuery<TTracked>);
        W.AfterDerived := Tracking.AliveNames;
      end)
    .&Then('la lista vive hasta que se suelta la derivada',
      procedure(W: TOwnedWorld)
      begin
        Expect(W.AfterFirst).ToEqual('Lista,A,B,C');
        Expect(W.AfterDerived).ToEqual('');
      end)

  .Scenario('Una consulta que nadie recorre')
    .Given('una lista con A, B y C',
      procedure(W: TOwnedWorld)
      begin
        FillABC(W);
        Expect(Tracking.AliveNames).ToEqual('Lista,A,B,C');
      end)
    .When('se construye una consulta con FromOwned y se suelta sin llamar a ningun terminal',
      procedure(W: TOwnedWorld)
      begin
        W.Query := Linq.FromOwned<TTracked>(W.Source);
        W.Query := Default(TQuery<TTracked>);
      end)
    .&Then('se liberan la lista y todos sus elementos',
      procedure(W: TOwnedWorld)
      begin
        Expect(Tracking.AliveNames).ToEqual('');
      end)

.Rule('Lo que un terminal entrega cambia de dueno')

  .Scenario('ToList sobre una lista duena')
    .Given('una fuente poseida de tres objetos y una lista de destino duena',
      procedure(W: TOwnedWorld)
      begin
        FillABC(W);
        W.Target := TTrackedList.Create('Destino');
        Expect(Tracking.AliveNames).ToEqual('Lista,A,B,C,Destino');
      end)
    .When('Where descarta B y ToList lleva los otros dos al destino',
      procedure(W: TOwnedWorld)
      begin
        Linq.FromOwned<TTracked>(W.Source).Where(NotB()).ToList(W.Target);
      end)
    .&Then('liberada la consulta, A y C siguen vivos en el destino',
      procedure(W: TOwnedWorld)
      begin
        Expect(AliveText([W.A, W.C])).ToEqual('True,True');
        Expect(W.Target.Count).ToEqual(2);
      end)

  .Scenario('La lista de destino libera lo que recibio')
    .Given('una lista duena que recibio A y C de una fuente poseida ya liberada',
      procedure(W: TOwnedWorld)
      begin
        FillABC(W);
        W.Target := TTrackedList.Create('Destino');
        Linq.FromOwned<TTracked>(W.Source).Where(NotB()).ToList(W.Target);
      end)
    .When('se libera la lista de destino',
      procedure(W: TOwnedWorld)
      begin
        // Only when the registry says the source let go of them: otherwise the source freed them
        // with itself, and freeing the list here would free them a second time.
        if Tracking.AliveNames = 'A,C,Destino' then
        begin
          W.Target.Free;
          W.Target := nil;
        end;
      end)
    .&Then('cada objeto se ha liberado una sola vez',
      procedure(W: TOwnedWorld)
      begin
        Expect(Tracking.AliveNames).ToEqual('');
      end)

  .Scenario('FirstOrDefault entrega solo lo que devuelve')
    .Given('una fuente poseida de tres objetos',
      procedure(W: TOwnedWorld)
      begin
        FillABC(W);
        Expect(Tracking.AliveNames).ToEqual('Lista,A,B,C');
      end)
    .When('se pide FirstOrDefault y se suelta la consulta',
      procedure(W: TOwnedWorld)
      begin
        W.Got := Linq.FromOwned<TTracked>(W.Source).FirstOrDefault;
      end)
    .&Then('el devuelto es A y sigue vivo, y los otros dos se han liberado',
      procedure(W: TOwnedWorld)
      begin
        Expect(BoolToStr(W.Got = W.A, True)).ToEqual('True');
        Expect(Tracking.AliveNames).ToEqual('A');
      end)

  .Scenario('Un terminal que no devuelve nada no entrega nada')
    .Given('una fuente poseida de dos objetos',
      procedure(W: TOwnedWorld)
      begin
        FillAB(W);
        Expect(Tracking.AliveNames).ToEqual('Lista,A,B');
      end)
    .When('se pide TrySingle y se suelta la consulta',
      procedure(W: TOwnedWorld)
      begin
        var Value: TTracked;
        W.Answers := BoolToStr(Linq.FromOwned<TTracked>(W.Source).TrySingle(Value), True);
      end)
    .&Then('devuelve False, y los dos objetos se han liberado',
      procedure(W: TOwnedWorld)
      begin
        Expect(W.Answers).ToEqual('False');
        Expect(Tracking.AliveNames).ToEqual('');
      end)

.Rule('Un destino que lanza al anadir no deja dos duenos')

  .Scenario('El destino lo guarda y despues lanza')
    .Given('una fuente poseida de tres objetos, y un destino dueno que lanza al recibir B despues de guardarlo',
      procedure(W: TOwnedWorld)
      begin
        FillABC(W);
        W.Target := TRaisingList.Create('Destino');
        TRaisingList(W.Target).Bad := W.B;
        Expect(Tracking.AliveNames).ToEqual('Lista,A,B,C,Destino');
      end)
    .When('ToList vuelca la consulta en ese destino',
      procedure(W: TOwnedWorld)
      begin
        try
          Linq.FromOwned<TTracked>(W.Source).ToList(W.Target);
        except
          on E: Exception do
            W.Raised := E.Message;
        end;
      end)
    .&Then('la excepcion llega al llamante, y A y B siguen vivos en el destino',
      procedure(W: TOwnedWorld)
      begin
        Expect(W.Raised).ToEqual('rechazo B');
        Expect(W.Target.Count).ToEqual(2);
        Expect(Tracking.AliveNames).ToEqual('A,B,Destino');
      end)

.Rule('Lo que nadie se lleva muere con la fuente')

  .Scenario('Lo descartado por Where')
    .Given('una fuente poseida de tres objetos y una lista de destino duena',
      procedure(W: TOwnedWorld)
      begin
        FillABC(W);
        W.Target := TTrackedList.Create('Destino');
        Expect(Tracking.AliveNames).ToEqual('Lista,A,B,C,Destino');
      end)
    .When('Where descarta B y ToList lleva los otros dos al destino',
      procedure(W: TOwnedWorld)
      begin
        Linq.FromOwned<TTracked>(W.Source).Where(NotB()).ToList(W.Target);
      end)
    .&Then('liberada la consulta, B se ha liberado',
      procedure(W: TOwnedWorld)
      begin
        Expect(AliveText([W.B])).ToEqual('False');
      end)

  .Scenario('Terminales que solo preguntan')
    .Given('una lista con A, B y C',
      procedure(W: TOwnedWorld)
      begin
        FillABC(W);
        Expect(Tracking.AliveNames).ToEqual('Lista,A,B,C');
      end)
    .When('se pregunta IsEmpty y Any a una consulta de fuente poseida y se suelta',
      procedure(W: TOwnedWorld)
      begin
        W.Query := Linq.FromOwned<TTracked>(W.Source);
        W.Answers := BoolToStr(W.Query.IsEmpty, True) + ',' + BoolToStr(W.Query.Any(
          function(X: TTracked): Boolean
          begin
            Result := X.Name = 'B';
          end), True);
        W.Query := Default(TQuery<TTracked>);
      end)
    .&Then('responden, y se han liberado todos sus elementos',
      procedure(W: TOwnedWorld)
      begin
        Expect(W.Answers).ToEqual('False,True');
        Expect(Tracking.AliveNames).ToEqual('');
      end)

.Rule('Fuera de los terminales, una consulta presta')

  .Scenario('Recorrer no entrega')
    .Given('una fuente poseida de tres objetos',
      procedure(W: TOwnedWorld)
      begin
        FillABC(W);
        Expect(Tracking.AliveNames).ToEqual('Lista,A,B,C');
      end)
    .When('se recorre con for..in y se suelta la consulta',
      procedure(W: TOwnedWorld)
      begin
        W.Seen := NamesOf(Linq.FromOwned<TTracked>(W.Source));
      end)
    .&Then('los recorre todos, y todos se han liberado',
      procedure(W: TOwnedWorld)
      begin
        Expect(W.Seen).ToEqual('A,B,C');
        Expect(Tracking.AliveNames).ToEqual('');
      end)

  .Scenario('Convertir a IEnumerable deja de entregar')
    .Given('una fuente poseida de tres objetos y una lista de destino que no posee',
      procedure(W: TOwnedWorld)
      begin
        FillABC(W);
        W.Target := TTrackedList.Create('Destino', False);
        Expect(Tracking.AliveNames).ToEqual('Lista,A,B,C,Destino');
      end)
    .When('la consulta pasa por IEnumerable y Linq.From la vuelca en el destino',
      procedure(W: TOwnedWorld)
      begin
        var Seq: IEnumerable<TTracked> := Linq.FromOwned<TTracked>(W.Source);
        Linq.From<TTracked>(Seq).ToList(W.Target);
      end)
    .&Then('liberados la consulta y el IEnumerable, todos los elementos se han liberado',
      procedure(W: TOwnedWorld)
      begin
        Expect(W.Target.Count).ToEqual(3);
        Expect(Tracking.AliveNames).ToEqual('Destino');
      end)

.Rule('Lo entregado no vuelve a salir')

  .Scenario('Dos ToList sobre la misma consulta')
    .Given('una fuente poseida de tres objetos y dos listas duenas',
      procedure(W: TOwnedWorld)
      begin
        FillABC(W);
        W.Target := TTrackedList.Create('Destino');
        W.Second := TTrackedList.Create('Segunda');
        Expect(Tracking.AliveNames).ToEqual('Lista,A,B,C,Destino,Segunda');
      end)
    .When('ToList lleva los tres a una lista, y otra vez ToList a la otra',
      procedure(W: TOwnedWorld)
      begin
        W.Query := Linq.FromOwned<TTracked>(W.Source);
        W.Query.ToList(W.Target);
        W.Query.ToList(W.Second);
        W.Query := Default(TQuery<TTracked>);
      end)
    .&Then('la segunda lista queda vacia, y los tres siguen vivos en la primera',
      procedure(W: TOwnedWorld)
      begin
        Expect(W.Second.Count).ToEqual(0);
        Expect(W.Target.Count).ToEqual(3);
        Expect(AliveText([W.A, W.B, W.C])).ToEqual('True,True,True');
      end)

  .Scenario('FirstOrDefault dos veces')
    .Given('una fuente poseida de dos objetos',
      procedure(W: TOwnedWorld)
      begin
        FillAB(W);
        Expect(Tracking.AliveNames).ToEqual('Lista,A,B');
      end)
    .When('se pide dos veces FirstOrDefault a la misma consulta y se suelta',
      procedure(W: TOwnedWorld)
      begin
        W.Query := Linq.FromOwned<TTracked>(W.Source);
        W.Got := W.Query.FirstOrDefault;
        W.Got2 := W.Query.FirstOrDefault;
        W.Query := Default(TQuery<TTracked>);
      end)
    .&Then('cada llamada devuelve un objeto distinto, y los dos siguen vivos',
      procedure(W: TOwnedWorld)
      begin
        Expect(BoolToStr((W.Got = W.A) and (W.Got2 = W.B), True)).ToEqual('True');
        Expect(Tracking.AliveNames).ToEqual('A,B');
      end)

.Rule('Solo cambian de dueno los objetos de la fuente')

  .Scenario('Proyectar a valores')
    .Given('una fuente poseida de tres objetos',
      procedure(W: TOwnedWorld)
      begin
        FillABC(W);
        Expect(Tracking.AliveNames).ToEqual('Lista,A,B,C');
      end)
    .When('se proyecta a string con Select y se materializa con ToArray',
      procedure(W: TOwnedWorld)
      begin
        W.Strings := Linq.FromOwned<TTracked>(W.Source).Select<string>(
          function(X: TTracked): string
          begin
            Result := X.Name;
          end).ToArray;
      end)
    .&Then('el array conserva las tres cadenas, y los tres objetos se han liberado',
      procedure(W: TOwnedWorld)
      begin
        Expect(string.Join(',', W.Strings)).ToEqual('A,B,C');
        Expect(Tracking.AliveNames).ToEqual('');
      end)

  .Scenario('Proyectar a objetos nuevos')
    .Given('una fuente poseida de tres objetos y una lista de destino duena',
      procedure(W: TOwnedWorld)
      begin
        FillABC(W);
        W.Target := TTrackedList.Create('Destino');
        Expect(Tracking.AliveNames).ToEqual('Lista,A,B,C,Destino');
      end)
    .When('Select crea un objeto nuevo por elemento y ToList los lleva al destino',
      procedure(W: TOwnedWorld)
      begin
        Linq.FromOwned<TTracked>(W.Source).Select<TTracked>(
          function(X: TTracked): TTracked
          begin
            Result := TTracked.Create(X.Name + '2');
          end).ToList(W.Target);
      end)
    .&Then('los de la fuente se han liberado, y los nuevos siguen vivos en el destino',
      procedure(W: TOwnedWorld)
      begin
        Expect(Tracking.AliveNames).ToEqual('Destino,A2,B2,C2');
      end)

.Rule('Una lista que no posee sus elementos')

  .Scenario('OwnsObjects desactivado')
    .Given('una lista que no posee sus tres objetos, cuyo dueno es otra lista',
      procedure(W: TOwnedWorld)
      begin
        W.Other := TTrackedList.Create('Duena');
        W.A := TTracked.Create('A');
        W.B := TTracked.Create('B');
        W.C := TTracked.Create('C');
        W.Other.AddRange([W.A, W.B, W.C]);
        W.Source := TTrackedList.Create('Lista', False);
        W.Source.AddRange([W.A, W.B, W.C]);
        W.Target := TTrackedList.Create('Destino', False);
        Expect(Tracking.AliveNames).ToEqual('Duena,A,B,C,Lista,Destino');
      end)
    .When('se consulta con FromOwned y ToList vuelca los tres en otra lista',
      procedure(W: TOwnedWorld)
      begin
        Linq.FromOwned<TTracked>(W.Source).ToList(W.Target);
      end)
    .&Then('liberada la consulta, la lista recibida se ha liberado, y ninguno de sus objetos',
      procedure(W: TOwnedWorld)
      begin
        Expect(Tracking.AliveNames).ToEqual('Duena,A,B,C,Destino');
      end)

.Rule('From sigue prestando')

  .Scenario('Reasignar una variable que guardaba una fuente poseida')
    .Given('una variable con una consulta de FromOwned sobre una lista A, y otra lista B que posee sus objetos',
      procedure(W: TOwnedWorld)
      begin
        W.Source := TTrackedList.Create('ListaA');
        W.Source.Add(TTracked.Create('A1'));
        W.Other := TTrackedList.Create('ListaB');
        W.Other.AddRange([TTracked.Create('B1'), TTracked.Create('B2')]);
        W.Target := TTrackedList.Create('Destino', False);
        W.Query := Linq.FromOwned<TTracked>(W.Source);
        Expect(Tracking.AliveNames).ToEqual('ListaA,A1,ListaB,B1,B2,Destino');
      end)
    .When('se le asigna una consulta de From sobre B y ToList la vuelca en otra lista',
      procedure(W: TOwnedWorld)
      begin
        W.Query := Linq.From<TTracked>(W.Other);
        W.Query.ToList(W.Target);
      end)
    .&Then('A y sus elementos se han liberado, y B conserva sus objetos',
      procedure(W: TOwnedWorld)
      begin
        Expect(Tracking.AliveNames).ToEqual('ListaB,B1,B2,Destino');
        Expect(W.Other.Count).ToEqual(2);
      end);

end.
