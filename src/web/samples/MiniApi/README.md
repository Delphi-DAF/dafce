# MiniApi — un controlador con enrutado por atributos, y sus propias peticiones

Una API de dos recursos servida por `TWebApplication`, que **se llama a sí misma** y escribe el
código y el cuerpo de cada respuesta. No hay que creerse lo que enseña: sale por pantalla.

## Qué enseña

- **Componer la variante Web son cuatro líneas.** `TWebApplication.CreateBuilder`, los servicios,
  `WebApp.AddHttpServer` y `Build`. `MapControllers` resuelve el router, y resolverlo **es**
  descubrirlos: el router recorre el RTTI buscando `[WebRoute]` al construirse.
- **Un segmento literal gana a un parámetro**, se declaren en el orden que se declaren. `Search` y
  `Health` están declaradas después de `GetById` y `Remove`, y `/api/products/search` llega a
  `Search`, no a `/{Id}`.
- **Un segmento `{Id}` entra por nombre en el parámetro**, convertido al tipo que el parámetro
  declare. `[FromQuery]` toma el valor de la cadena de consulta, y un parámetro ausente llega
  vacío — decidir si eso es un `400` es de la acción.
- **Una ruta que ningún controlador declara contesta `404 Not found`.** No lo contesta una acción
  sino el módulo web de la variante, así que `/api/products/99` —que sí casa, y cuya acción decide—
  y `/api/nothing-declared` dan el mismo código por caminos distintos.
- **`mtAny` contesta a cualquier verbo**, y dos acciones con la misma ruta y distinto verbo son dos
  acciones: `GET /api/products/42` devuelve el producto y `DELETE /api/products/42` devuelve 204.
- **`Ok(TObject)` serializa a JSON y no se queda con el objeto.** `TJsonResult` lo convierte a
  texto en el constructor, así que el llamante sigue siendo su dueño y lo libera — ver `GetById`.
- **El controlador lo construye el router**, resolviendo sus dependencias del contenedor y
  liberándolo al acabar la petición. Cada petición corre en su propio ámbito.
- **El sample arranca el servidor a mano.** Una aplicación de verdad llama a `App.Run`, que arranca
  el host, arranca el servidor y espera la parada; aquí se usan `Server.Start` y `Server.Stop` para
  poder hacer unas peticiones y salir.
- **`hoNoProtocolErrorException` en el cliente.** Sin esa opción `TIdHTTP` lanza en cualquier
  código que no sea 2xx, y entonces el sample no podría enseñar ni el 404 ni el 400 — que son la
  mitad de lo que hay que ver.

## Ejecutar

Ocupa el puerto **8099**; si está cogido, cambiar `Port` en el `.dpr`.

```
BDS make --project:src/web/samples/MiniApi/MiniApi.dproj
out\Win32\samples\bin\MiniApi.exe
```

Salida:

```
escuchando en http://127.0.0.1:8099

== Un segmento de la ruta entra por nombre en el parametro
  GET    /api/products/42             -> 200 {"id":42,"name":"Taladro","price":89.9}

== Lo que no existe contesta 404, y lo dice
  GET    /api/products/99             -> 404 no hay producto 99
  GET    /api/nothing-declared        -> 404 Not found

== De la cadena de consulta, con FromQuery
  GET    /api/products/search?q=taladro -> 200 buscando "taladro" entre 2 producto(s)
  GET    /api/products/search         -> 400 falta el parametro q

== mtAny contesta a cualquier verbo
  GET    /api/products/health         -> 200 true
  DELETE /api/products/health         -> 200 true

== La misma ruta, otro verbo, otra accion
  DELETE /api/products/42             -> 204
```

El 204 llega sin cuerpo: es lo que significa *No Content*, aunque `NoContent` acepte un mensaje.
