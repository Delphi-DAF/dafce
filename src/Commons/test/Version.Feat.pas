unit Version.Feat;

// Las clases con atributo viven en la seccion interface a proposito: lo que se ejerce aqui es una
// lectura por RTTI, y un tipo que el enlazador no considere alcanzable deja el atributo fuera. Por
// la misma razon el runner declara STRONGLINKTYPES.

interface

uses
  Daf.Application.Version;

type
  [Version('1.2.3-rc.2+build.7', 'SpecApp', 'DAF')]
  TVersionedApp = class
  end;

  // Sin atributo: es el caso que decide si "sin atributo, el record esta vacio" es verdad.
  TUnversionedApp = class
  end;

implementation

uses
  Daf.MiniSpec;

type
  /// El Given nombra la clase que se va a leer y el When la lee: con `GetFrom` en el Given, un
  /// fallo de la lectura se informa como fallo del montaje, y el Then solo puede comparar lo que
  /// la propia preparacion dejo escrito.
  TVersionWorld = class
  public
    AppClass: TClass;
    Info: TVersionInfo;
    Empty: TVersionInfo;
  end;

initialization

Feature('''
Feature Los metadatos de version declarados en una clase @commons @version

  Como quien despliega una aplicacion DAF
  Quiero que la version que declare sea la que la aplicacion informa
  Para que un incidente se pueda atribuir a una build
''')

.UseWorld<TVersionWorld>

.Rule('La version declarada es la que se informa')

  .Scenario('Tag devuelve el tag declarado en el atributo')
    .Given('una clase que declara su version',
      procedure(W: TVersionWorld)
      begin
        W.AppClass := TVersionedApp;
      end)
    .When('se leen sus metadatos de version',
      procedure(W: TVersionWorld)
      begin
        W.Info := TVersionInfo.GetFrom(W.AppClass);
      end)
    .&Then('Tag es exactamente lo que dice el atributo',
      procedure(W: TVersionWorld)
      begin
        Expect(W.Info.Tag).ToEqual('1.2.3-rc.2+build.7');
      end)

  .Scenario('El semver declarado se parte en sus piezas')
    .Given('una clase que declara su version',
      procedure(W: TVersionWorld)
      begin
        W.AppClass := TVersionedApp;
      end)
    .When('se leen sus metadatos de version',
      procedure(W: TVersionWorld)
      begin
        W.Info := TVersionInfo.GetFrom(W.AppClass);
      end)
    .&Then('las piezas del semver son las del tag',
      procedure(W: TVersionWorld)
      begin
        Expect(W.Info.Major).ToEqual(1);
        Expect(W.Info.Minor).ToEqual(2);
        Expect(W.Info.Patch).ToEqual(3);
        Expect(W.Info.PreRelease).ToEqual('rc.2');
        Expect(W.Info.Metadata).ToEqual('build.7');
      end)
    .&And('y el nombre y la compania son los declarados',
      procedure(W: TVersionWorld)
      begin
        Expect(W.Info.ApplicationName).ToEqual('SpecApp');
        Expect(W.Info.Company).ToEqual('DAF');
      end)

  .Scenario('Tag y VersionTag cuentan la misma version')
    .Given('una clase que declara su version',
      procedure(W: TVersionWorld)
      begin
        W.AppClass := TVersionedApp;
      end)
    .When('se leen sus metadatos de version',
      procedure(W: TVersionWorld)
      begin
        W.Info := TVersionInfo.GetFrom(W.AppClass);
      end)
    .&Then('el tag declarado y el reconstruido coinciden',
      procedure(W: TVersionWorld)
      begin
        // Tag es la cadena declarada; VersionTag se rearma desde las piezas parseadas. Que
        // discrepen es el sintoma de que una de las dos no se rellena.
        Expect(W.Info.VersionTag).ToEqual(W.Info.Tag);
      end)

.Rule('Una clase que no declara version informa una version vacia')

  // GetFrom deja Result intacto cuando no hay atributo, asi que esto mide si "sin atributo, el
  // record esta vacio" es cierto o solo probable.
  .Scenario('Sin atributo, el record esta vacio y no lleva basura')
    .Given('una clase que no declara version',
      procedure(W: TVersionWorld)
      begin
        W.AppClass := TUnversionedApp;
      end)
    .When('se leen sus metadatos de version',
      procedure(W: TVersionWorld)
      begin
        W.Empty := TVersionInfo.GetFrom(W.AppClass);
      end)
    .&Then('no hay tag, ni nombre, ni compania',
      procedure(W: TVersionWorld)
      begin
        Expect(W.Empty.Tag).ToEqual('');
        Expect(W.Empty.ApplicationName).ToEqual('');
        Expect(W.Empty.Company).ToEqual('');
      end)
    .&And('y las piezas numericas son cero',
      procedure(W: TVersionWorld)
      begin
        Expect(W.Empty.Major).ToEqual(0);
        Expect(W.Empty.Minor).ToEqual(0);
        Expect(W.Empty.Patch).ToEqual(0);
      end);

end.
