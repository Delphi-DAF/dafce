# SQLCute — Constructor Fluido de SQL para Delphi

**🌍 Idioma: [English](README.md) | Español**

Construye consultas SQL con una API fluida, segura en tipos. Sin concatenación de cadenas, sin SQL crudo — solo encadenamiento de métodos que compila a SQL parametrizado.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![Licencia](https://img.shields.io/badge/licencia-MIT-blue.svg)](../../legal/LICENSE.md)

---

## Conceptos

| Concepto | Descripción |
|----------|-------------|
| `IQuery` | Interfaz fluida — cada método devuelve `IQuery` para encadenamiento |
| `TQuery.New` | Punto de entrada — crea una query con conteo de referencias, sin necesidad de `Free` |
| `TSQLResult` | Resultado: cadena `SQL` + array `Bindings` de parámetros posicionales |
| `IQueryCompiler` | Estrategia para generación de SQL específica del dialecto (ANSI por defecto, o un compilador de dialecto) |

---

## Inicio Rápido

```pascal
uses Daf.SQLCute;

var Q := TQuery.New
  .From('pedidos')
  .Select(['id', 'total', 'estado'])
  .Where('estado', 'pendiente')
  .Where('total', '>', 100)
  .OrderByDesc('creado_en')
  .Limit(20);

var R := TAnsiSqlCompiler.Create.Compile(Q);
// R.SQL      → 'SELECT id, total, estado FROM pedidos WHERE estado = ? AND total > ? ORDER BY creado_en DESC LIMIT 20'
// R.Bindings → ['pendiente', 100]
```

---

## Patrones Comunes

### Filtrar y paginar

```pascal
TQuery.New
  .From('usuarios')
  .Where('activo', True)
  .ForPage(Pagina, 25)   // LIMIT 25 OFFSET (Pagina-1)*25
```

### JOIN

```pascal
TQuery.New
  .From('pedidos')
  .Join('clientes', 'pedidos.cliente_id', 'clientes.id')
  .Select(['pedidos.id', 'clientes.nombre'])
```

### Agregación

```pascal
TQuery.New
  .From('ventas')
  .GroupBy('producto_id')
  .SelectCount('*', 'qty')
  .SelectSum('importe', 'total')
  .Having('count(*)', '>', 10)
```

### DML

```pascal
// INSERT
TQuery.New.From('logs')
  .AsInsert(['nivel', 'mensaje'], ['error', 'disco lleno']);

// UPDATE
TQuery.New.From('usuarios')
  .Where('id', 42)
  .AsUpdate(['email'], ['nuevo@ejemplo.com']);

// DELETE
TQuery.New.From('sesiones')
  .Where('expirado', True)
  .AsDelete;
```

### Compilador de dialecto

```pascal
uses Daf.SQLCute.Compiler.Postgres;

var R := TPostgresCompiler.Create.Compile(Q);
// identificadores entrecomillados con "", marcadores $1 $2 …
```

---

## Documentación

| Recurso | Descripción |
|---------|-------------|
| [**Guía de Uso**](docs/GUIDE.es.md) | Referencia completa de la API con todos los métodos |
| [SELECT & FROM](docs/guide/select.es.md) | Columnas, DISTINCT, agregados, ORDER BY, LIMIT |
| [WHERE](docs/guide/where.es.md) | Condiciones, IN, EXISTS, grupos, LIKE, operaciones de fecha |
| [JOINs](docs/guide/joins.es.md) | Todos los tipos de join y callbacks |
| [GROUP BY / HAVING](docs/guide/groupby.es.md) | Agrupación y filtros de agregados |
| [Operaciones de Conjunto](docs/guide/setops.es.md) | UNION, INTERSECT, EXCEPT, paginación |
| [DML](docs/guide/dml.es.md) | INSERT, UPDATE, DELETE |
| [Subconsultas y CTEs](docs/guide/subqueries.es.md) | Tablas derivadas, WITH, CTEs recursivos |
| [Operaciones de Cadena](docs/guide/string-ops.es.md) | LIKE, starts/ends/contains |
| [Operaciones de Fecha](docs/guide/date-ops.es.md) | WhereDate, casting por dialecto |
| [Compiladores de Dialecto](docs/guide/dialects.es.md) | Postgres, MySQL, SQLite, SQL Server, Oracle, Firebird |
| [**Paquete Compilers**](../SQLCute.Compilers/README.es.md) | Clases de compiladores de dialecto |
