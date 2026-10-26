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

  // Un tag que no sigue la gramatica semver. Entender el texto sirve para poder responder por sus
  // piezas, no para sustituirlo: lo que esta clase mide es que el texto declarado sobreviva.
  [Version('v2.0.0', 'SpecApp', 'DAF')]
  TOddlyVersionedApp = class
  end;

  // Semver sin seccion de metadatos: la otra mitad del par que mide como se compone el build.
  [Version('2.0.0', 'SpecApp', 'DAF')]
  TPlainVersionedApp = class
  end;

  [Version('2.0.0', 'SpecApp', 'DAF', True)]
  TDebugDeclaredApp = class
  end;

  // El runner enlaza el framework en Debug: si Debug saliera del framework, esta clase diria True.
  [Version('2.0.0', 'SpecApp', 'DAF', False)]
  TReleaseDeclaredApp = class
  end;

implementation

uses
  System.SysUtils,
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
    .&And('y se declara semver, que es lo que hace fiables esas piezas',
      procedure(W: TVersionWorld)
      begin
        Expect(W.Info.IsSemVer).ToBeTrue;
        Expect(W.Info.IsEmpty).ToBeFalse;
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
      end)

  .Scenario('Sin atributo, la version informada es vacia y no una version en ceros')
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
    .&Then('la version informada es la cadena vacia',
      procedure(W: TVersionWorld)
      begin
        // "No hay version" y "la version es 0.0.0" son estados distintos, y quien lo lee tiene que
        // poder separarlos: uno no se anuncia, el otro si.
        Expect(W.Empty.VersionTag).ToEqual('');
      end)
    .&And('y se declara vacia, no semver',
      procedure(W: TVersionWorld)
      begin
        Expect(W.Empty.IsEmpty).ToBeTrue;
        Expect(W.Empty.IsSemVer).ToBeFalse;
      end)

.Rule('Un tag que no es semver se informa tal cual')

  .Scenario('El texto declarado sobrevive aunque no se entienda')
    .Given('una clase que declara una version que no sigue la gramatica semver',
      procedure(W: TVersionWorld)
      begin
        W.AppClass := TOddlyVersionedApp;
      end)
    .When('se leen sus metadatos de version',
      procedure(W: TVersionWorld)
      begin
        W.Info := TVersionInfo.GetFrom(W.AppClass);
      end)
    .&Then('el tag declarado y el informado coinciden',
      procedure(W: TVersionWorld)
      begin
        // Es el gemelo de "Tag y VersionTag cuentan la misma version": alli el tag es valido y la
        // igualdad se cumple sola. Aqui es donde la invariante se gana o se pierde.
        Expect(W.Info.VersionTag).ToEqual(W.Info.Tag);
      end)
    .&And('y dice que no es semver, sin declararse vacia',
      procedure(W: TVersionWorld)
      begin
        // Los dos estados se distinguen: hay version, y no se entendio.
        Expect(W.Info.IsSemVer).ToBeFalse;
        Expect(W.Info.IsEmpty).ToBeFalse;
      end)

.Rule('Los metadatos de build son una sola seccion')

  .Scenario('Un tag sin metadatos abre la seccion con los de compilacion')
    .Given('una clase cuya version no declara metadatos',
      procedure(W: TVersionWorld)
      begin
        W.AppClass := TPlainVersionedApp;
      end)
    .When('se leen sus metadatos de version',
      procedure(W: TVersionWorld)
      begin
        W.Info := TVersionInfo.GetFrom(W.AppClass);
      end)
    .&Then('hay exactamente una seccion de build',
      procedure(W: TVersionWorld)
      begin
        // Se cuenta la seccion, no se fija su contenido: el mismo runner corre en Win32 y en
        // Win64, y siempre en Debug, asi que la cadena completa seria distinta en cada pasada.
        Expect(W.Info.VersionTag(VersionTemplateWithBuild).CountChar('+')).ToEqual(1);
      end)

  .Scenario('Un tag que ya trae metadatos no abre una segunda seccion')
    .Given('una clase cuya version declara sus propios metadatos',
      procedure(W: TVersionWorld)
      begin
        W.AppClass := TVersionedApp;
      end)
    .When('se leen sus metadatos de version',
      procedure(W: TVersionWorld)
      begin
        W.Info := TVersionInfo.GetFrom(W.AppClass);
      end)
    .&Then('sigue habiendo exactamente una seccion de build',
      procedure(W: TVersionWorld)
      begin
        Expect(W.Info.VersionTag(VersionTemplateWithBuild).CountChar('+')).ToEqual(1);
      end)
    .&And('y los declarados y los de compilacion van unidos dentro de ella',
      procedure(W: TVersionWorld)
      begin
        // El punto es el separador interno de la seccion; lo que venga detras es la plataforma.
        Expect(W.Info.VersionTag(VersionTemplateWithBuild)).ToContain('+build.7.');
      end)

.Rule('La configuracion de depuracion la declara el binario')

  .Scenario('Sin declararla no se afirma una build de depuracion')
    .Given('una clase que declara su version sin decir si es de depuracion, en un runner que enlaza el framework en Debug',
      procedure(W: TVersionWorld)
      begin
        W.AppClass := TPlainVersionedApp;
      end)
    .When('se leen sus metadatos de version',
      procedure(W: TVersionWorld)
      begin
        W.Info := TVersionInfo.GetFrom(W.AppClass);
      end)
    .&Then('no dice que sea una build de depuracion',
      procedure(W: TVersionWorld)
      begin
        Expect(W.Info.Debug).ToBeFalse;
      end)
    .&And('y su seccion de build no lo anuncia',
      procedure(W: TVersionWorld)
      begin
        Expect(Pos('.Dbg', W.Info.VersionTag(VersionTemplateWithBuild))).ToEqual(0);
      end)

  .Scenario('Una build que se declara de depuracion lo dice en su seccion de build')
    .Given('una clase que declara su version y una build de depuracion',
      procedure(W: TVersionWorld)
      begin
        W.AppClass := TDebugDeclaredApp;
      end)
    .When('se leen sus metadatos de version',
      procedure(W: TVersionWorld)
      begin
        W.Info := TVersionInfo.GetFrom(W.AppClass);
      end)
    .&Then('dice que es una build de depuracion',
      procedure(W: TVersionWorld)
      begin
        Expect(W.Info.Debug).ToBeTrue;
      end)
    .&And('y la seccion de build acaba anunciandolo',
      procedure(W: TVersionWorld)
      begin
        Expect(W.Info.VersionTag(VersionTemplateWithBuild)).ToEndWith('.Dbg');
      end)

  .Scenario('Una build que se declara de release no lo dice, aunque el framework este en Debug')
    .Given('una clase que declara su version y que no es una build de depuracion',
      procedure(W: TVersionWorld)
      begin
        W.AppClass := TReleaseDeclaredApp;
      end)
    .When('se leen sus metadatos de version',
      procedure(W: TVersionWorld)
      begin
        W.Info := TVersionInfo.GetFrom(W.AppClass);
      end)
    .&Then('no dice que sea una build de depuracion',
      procedure(W: TVersionWorld)
      begin
        Expect(W.Info.Debug).ToBeFalse;
        Expect(Pos('.Dbg', W.Info.VersionTag(VersionTemplateWithBuild))).ToEqual(0);
      end)

.Rule('El formato del texto lo decide quien lo muestra')

  .Scenario('Sin plantilla se usa la de por defecto')
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
    .&Then('informa lo mismo que pasando la plantilla por defecto a mano',
      procedure(W: TVersionWorld)
      begin
        Expect(W.Info.VersionTag).ToEqual(W.Info.VersionTag(VersionTemplate));
      end)

  .Scenario('Una plantilla del llamante sustituye cada marcador por su pieza')
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
    .&Then('cada marcador trae la pieza que nombra',
      procedure(W: TVersionWorld)
      begin
        // Las piezas crudas no llevan separador: quien escribe la plantilla compone.
        Expect(W.Info.VersionTag('${Major}|${Minor}|${Patch}|${Metadata}')).ToEqual('1|2|3|build.7');
      end);

end.
