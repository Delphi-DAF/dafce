# Ejemplo de Servicio Hospedado en DAF

Este ejemplo demuestra cómo implementar y utilizar servicios hospedados en DAF (Delphi Application Framework), mostrando diferentes patrones de servicios en segundo plano y su ciclo de vida.

## Estructura del Proyecto

El proyecto está organizado en varios archivos que muestran diferentes tipos de servicios hospedados:

- `HostedService.dpr`: Punto de entrada de la aplicación
- `HostedWorker.pas`: Implementación de servicios hospedados
- `Files/`: Directorio con archivos de configuración
- `HostedServiceWithTimer.dpr`: Ejemplo adicional con temporizador

## Tipos de Servicios Hospedados

### 1. Servicio de Ejecución Continua
```pascal
TBackgroundWorker = class(TInterfacedObject, IHostedService)
  // Servicio que se ejecuta continuamente en segundo plano
  // Implementa un bucle de procesamiento
end;
```

### 2. Servicio con Temporizador
```pascal
TTimerHostedService = class(TInterfacedObject, IHostedService)
  // Servicio que se ejecuta en intervalos regulares
  // Utiliza un temporizador para programar tareas
end;
```

## Características Implementadas

### 1. Ciclo de Vida de Servicios
- Inicialización ordenada
- Ejecución controlada
- Finalización graceful
- Manejo de cancelación

### 2. Patrones de Ejecución
- Ejecución continua
- Ejecución programada
- Ejecución con temporizador
- Manejo de errores

### 3. Integración con el Host
- Registro de servicios
- Control de dependencias
- Configuración
- Logging

## Ejemplo de Implementación

```pascal
// Servicio de ejecución continua
type
  TBackgroundWorker = class(TInterfacedObject, IHostedService)
  private
    FStopping: Boolean;
  public
    procedure Start;
    procedure Stop;
  end;

// Servicio con temporizador
type
  TTimerHostedService = class(TInterfacedObject, IHostedService)
  private
    FTimer: TTimer;
  public
    procedure Start;
    procedure Stop;
  end;
```

## Configuración del Host

```pascal
var B := TCliApplication.CreateBuilder(CommandLineConfig.ProcessArgs);

// El entorno ya responde al volver del constructor: no hace falta un callback para leerlo.
var JsonFilePath := TPath.Combine(B.Environment.BinPath, B.Environment.ApplicationName,
                                  'appsettings.json');
JsonConfig.AddFile(B.Configuration, JsonFilePath);

// Registrar servicios hospedados
B.Services.AddHostedService<TBackgroundWorker>;
B.Services.AddHostedService<TTimerHostedService>;

// El proveedor de log se compone leyendo la configuración que se acaba de añadir.
B.Logging.AddProvider(TNNLogLoader.BuildProvider(
  B.Environment, B.Configuration.GetSection('Logging:NLog')));

ExitCode := B.Build.Run;
```

## Flujo de Ejecución

1. **Inicio del Servicio**:
   - Inicialización de recursos
   - Configuración del servicio
   - Inicio de la ejecución

2. **Ejecución**:
   - Procesamiento de tareas
   - Manejo de errores
   - Registro de eventos

3. **Detención**:
   - Señal de detención
   - Finalización de tareas en curso
   - Liberación de recursos

## Beneficios del Diseño

1. **Confiabilidad**: Manejo robusto de errores y recuperación
2. **Mantenibilidad**: Separación clara de responsabilidades
3. **Escalabilidad**: Fácil adición de nuevos servicios
4. **Testabilidad**: Servicios aislados y verificables

## Ejecución del Ejemplo

1. Compilar y ejecutar la aplicación
2. Los servicios mostrarán:
   - Mensajes de inicio/detención
   - Progreso de ejecución
   - Eventos y errores

## Consideraciones para Producción

### 1. Manejo de Errores
- Implementar reintentos
- Logging detallado
- Notificaciones de error
- Recuperación automática

### 2. Monitoreo
- Health checks
- Métricas de rendimiento
- Estado del servicio
- Uso de recursos

### 3. Configuración
- Parámetros ajustables
- Diferentes entornos
- Secretos y credenciales
- Timeouts y límites

## Notas Adicionales

- Los servicios pueden ser utilizados en aplicaciones de consola o servicios de Windows
- Es posible implementar servicios distribuidos
- Se pueden agregar mecanismos de coordinación entre servicios
- El diseño permite fácil migración a contenedores
- Considerar el uso de patrones como Circuit Breaker o Retry

## Mejores Prácticas

1. **Diseño de Servicios**:
   - Mantener servicios pequeños y enfocados
   - Implementar interfaces claras
   - Documentar comportamiento esperado
   - Considerar concurrencia

2. **Manejo de Recursos**:
   - Liberar recursos apropiadamente
   - Implementar timeouts
   - Manejar conexiones eficientemente
   - Monitorear uso de memoria

3. **Logging y Diagnóstico**:
   - Usar niveles de log apropiados
   - Incluir contexto relevante
   - Implementar trazas distribuidas
   - Facilitar debugging

# HostedServiceWithTimer Sample

Este ejemplo muestra cómo implementar un `IHostedService` con un bucle controlado por temporizador usando `TThread` y `TEvent`.

## Objetivo

- Ejecutar una tarea periódica en segundo plano
- Detenerla limpiamente cuando la aplicación finaliza


## Resultado esperado

```
🟢 TTimedWorker.Start called
[10:32:01] TTimedWorker running...
[10:32:02] TTimedWorker running...
...
🔴 TTimedWorker.Stop called
```

## Casos de uso

- Workers periódicos
- Monitores de sistema
- Polling de APIs, colas, etc.

## Requisitos

- Delphi
- DAF con soporte para `IHostedService`
- 
# 🗭 DAF – ¿Cuándo usar `HostedService` y cuándo usar `MediatR`?

Una de las decisiones clave al desarrollar aplicaciones modernas con DAF es elegir **el patrón adecuado para ejecutar lógica de infraestructura o negocio**. Esta guía te ayuda a decidir entre usar un `HostedService` o un `MediatR Handler`.

---

## 🗱 HostedService

Un `HostedService` representa un **servicio de infraestructura de larga duración**, que:

- Se **inicia automáticamente** al arrancar la aplicación.
- Tiene **ciclo de vida propio** (`Start` / `Stop`).
- Puede ejecutarse en **hilo de fondo** (con `TTask`) o directamente.

### ✅ Cuándo usarlo

- Polling de base de datos, colas o servicios externos.
- Timers recurrentes (por ejemplo: cada 5 segundos).
- Servicios que necesitan vivir toda la vida de la app (caches, watchdogs).
- Tareas programadas o automáticas que no dependen de un trigger de dominio.

### 📌 Ejemplo

```delphi
type
  TEmailSyncWorker = class(THostedServiceBase)
  protected
    procedure Execute(const Token: ICancellationToken); override;
  end;
```

---

## 🎯 MediatR

`MediatR` implementa un patrón de mensajería para representar **acciones discretas o eventos del dominio**. Es ideal para modelar:

- Intenciones del usuario o sistema → `IRequest` (comandos).
- Notificaciones de eventos → `INotification`.

### ✅ Cuándo usarlo

- Cuando una acción debe ejecutarse **una vez y de forma explícita**.
- Si forma parte de la **capa de aplicación o dominio**.
- Cuando se dispara a raíz de un controlador, un cambio de estado o un evento.
- Para separar lógica de infraestructura del negocio.

### 📌 Ejemplo

```delphi
type
  TRefreshCacheCommand = class(TInterfacedObject, IRequest)
  end;

type
  TRefreshCacheHandler = class(TInterfacedObject, IRequestHandler<TRefreshCacheCommand>)
    procedure Handle(const Request: TRefreshCacheCommand);
  end;
```

---

## 🧠 Reglas prácticas

| Si... | Usa... |
|-------|--------|
| Necesita ejecutarse periódicamente | `HostedService` |
| Necesita hilo de fondo o cancelación por token | `HostedService` |
| Es una acción explícita disparada por la app | `MediatR Command` |
| Es una reacción a un evento | `MediatR Notification` |
| Se ejecuta solo una vez | `MediatR` |
| Necesita iniciarse automáticamente | `HostedService` |

---

## 📄 Consejo Delphi

Delphi no tiene `async/await` como en .NET, así que los `HostedServices` con `TTask.Run` deben:
- Capturar excepciones (`try..except`)
- Cancelarse con `ICancellationToken`
- Llamar a `Wait` en `Stop` para garantizar limpieza

---

## 📂 Conclusión

Usar correctamente `HostedService` y `MediatR` mejora:

- La claridad del diseño.
- El mantenimiento de la solución.
- La testabilidad de cada componente.

Ambos son herramientas potentes dentro de DAF. Elegir bien cuál aplicar hace tu código más limpio y tu arquitectura más robusta.

