# ConfigSample — la cadena de proveedores y quién gana

Dos demostraciones que corren una detrás de otra: la configuración en memoria y la que sale de
ficheros. Cada una es una `class procedure Run`, y el `.dpr` espera un `Readln` entre ellas.

## Qué enseña

### En memoria — `InMemoryConfig.pas`

- `MemoryConfig.AddCollection(Builder, Dict)` **se queda con el diccionario**: quien lo crea no lo
  libera.
- Las claves jerárquicas se componen con `:` — `App:Title`, `App:Version` — igual que en cualquier
  otra fuente.
- El indexador de `IConfigurationRoot` también **escribe**: `Config['App:Title'] := '…'` cambia el
  valor en memoria y no toca el origen.

### Desde ficheros — `FileConfig.pas`

- `JsonConfig.AddFile` e `IniConfig.AddFile` se añaden **al mismo builder**, y se llama a `Build`
  después de cada uno. Así el segundo volcado enseña las dos fuentes fundidas, con el INI ganando
  las claves que repite: `Port` es `8088` en el JSON y `8080` en el INI.
- Un INI expresa la misma jerarquía que un JSON con el nombre de la sección: `[]` para las claves de
  raíz, y `[AppSettings:Users:0]` para el primer elemento de un array.
- `Dump` recorre el árbol con `Config.GetChildren` y `Section.HasChildren`, que es cómo se distingue
  una sección que agrupa de una clave con valor.
- El enlace a objeto es `TConfigurationBinder.Bind<TAppSettings>(Config, 'AppSettings', boFields)`:
  `boFields` porque la clase expone campos públicos, y la sobrecarga genérica construye el objeto y
  se lo entrega a quien llama, que es quien lo libera.

### Anidados, y quién libera qué

`TAppSettings` tiene un `TObjectList<TUser>` y cada `TUser` un `TList<string>`. Los dos los crea su
propio constructor y los libera su destructor: el enlazador **reutiliza** el objeto miembro que ya
está construido en vez de sustituirlo, así que esa es la forma que funciona. Y los dos llevan
`{$M+}`.

## Ejecutar

```
BDS make --project:src/Configuration/samples/ConfigSample/ConfigExample.dproj
out\Win32\samples\bin\ConfigExample.exe
```

Pide un `Readln` después de cada demostración, así que es interactivo.
