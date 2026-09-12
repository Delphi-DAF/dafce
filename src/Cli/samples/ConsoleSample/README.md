# Ejemplo de Aplicación de Consola en DAF

Este ejemplo demuestra cómo crear una aplicación de consola utilizando DAF (Delphi Application Framework), incorporando características como configuración, logging y servicios hospedados.

## Estructura del Proyecto

El proyecto está organizado en varios archivos que muestran diferentes aspectos de una aplicación de consola:

- `ConsoleSample.dpr`: Punto de entrada de la aplicación
- `ConsoleSample.Greeter.pas`: Implementación del servicio de saludo
- `ConsoleSample.AppSettings.pas`: Configuración de la aplicación
- `Files/`: Directorio con archivos de configuración

## Características Implementadas

### 1. Servicios Hospedados
El ejemplo implementa un servicio de saludo que:
- Se ejecuta como un servicio hospedado
- Utiliza configuración para personalizar los mensajes
- Demuestra el ciclo de vida de un servicio

### 2. Configuración
- Carga de configuración desde archivos
- Uso de valores por defecto
- Configuración específica de la aplicación

### 3. Logging
- Registro de eventos de la aplicación
- Diferentes niveles de logging
- Formato personalizado de mensajes

## Componentes Principales

### Servicio de Saludo
```pascal
TGreeterService = class(TInterfacedObject, IHostedService)
  // Servicio que genera saludos personalizados
  // Implementa el ciclo de vida de un servicio hospedado
end;
```

### Configuración de la Aplicación
```pascal
TAppSettings = class
  // Configuración específica de la aplicación
  // Incluye opciones como mensajes y tiempos de espera
end;
```

## Flujo de la Aplicación

1. **Inicio**:
   - Carga de configuración
   - Inicialización de servicios
   - Configuración de logging

2. **Ejecución**:
   - Inicio de servicios hospedados
   - Procesamiento de mensajes
   - Registro de eventos

3. **Finalización**:
   - Detención ordenada de servicios
   - Liberación de recursos
   - Cierre de logs

## Ejemplo de Uso

```pascal
// Crear y configurar el builder
var B := TCliApplication.CreateBuilder(CommandLineConfig.ProcessArgs);

// El entorno ya responde al volver del constructor: no hace falta un callback para leerlo.
var SampleFilesPath := TPath.Combine(B.Environment.BinPath, B.Environment.ApplicationName);
JsonConfig.AddFile(B.Configuration, TPath.Combine(SampleFilesPath, 'appsettings.json'));

B.Services.AddSingleton<IGreeter, TGreeter>;

// Ejecutar la aplicación
ExitCode := B.Build.Run;
```

## El programa completo

Lo de arriba es el fragmento que explica la forma. Este es el `.dpr` que compila, íntegro:

<!-- quickstart: src/Cli/samples/ConsoleSample/ConsoleSample.dpr -->

```pascal
program ConsoleSample;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.IOUtils,
  Daf.Rtti,
  Daf.Extensions.Hosting,
  Daf.Threading,
  Daf.Extensions.Configuration,
  Daf.Extensions.DependencyInjection,
  Daf.DependencyInjection.ActivatorUtilities,
  Daf.Configuration.Builder,
  Daf.Options.Configuration,
  Daf.Configuration.Json,
  Daf.CommandLine,
  Daf.Configuration.CommandLine,
  Daf.Cli,

  ConsoleSample.AppSettings in 'ConsoleSample.AppSettings.pas',
  ConsoleSample.Greeter in 'ConsoleSample.Greeter.pas';

// El caso que motivaba el estilo diferido: LEER configuracion para decidir que configuracion
// cargar. Costaba un callback, porque solo dentro de el existia el entorno resuelto. Ahora el
// builder ya responde en cuanto se crea, y la composicion se lee de arriba abajo.
begin
  ReportMemoryLeaksOnShutdown := True;

  var B := TCliApplication.CreateBuilder(CommandLineConfig.ProcessArgs);

  var SampleFilesPath := TPath.Combine(B.Environment.BinPath, B.Environment.ApplicationName);
  JsonConfig.AddFile(B.Configuration, TPath.Combine(SampleFilesPath, 'appsettings.json'));
  TShutdownHook.PresCtlCMsg := 'Pulse ctl-c para salir';

  B.Services.AddTransient<IWrapperImpl, TWrapper>;
  // Los ajustes se declaran una vez, y el contenedor los construye, los posee y los libera.
  // El servicio los pide por constructor como cualquier otra dependencia: ni lambda, ni
  // Free. El `as IConfiguration` es porque el gestor de configuracion es a la vez lector y
  // constructor, y Delphi solo admite un padre por interfaz.
  ConfigOptions.Bind<TAppSettings>(B.Services, B.Configuration as IConfiguration);
  B.Services.AddSingleton<IGreeter, TGreeter>;

  B.OnExecute(
    function(const Services: IServiceProvider; const Parsed: ICommandLineParseResult): Integer
    begin
      Services.GetService<IGreeter>.Greet;

      // CreateInstance devuelve un objeto NO contado: el dueño es quien llama. Sin este Free se
      // fugan DOS cosas, no una -- el accesor, y el IWrapperImpl que sostiene dentro, porque su
      // campo de interfaz no se finaliza nunca y el refcount no baja a cero.
      var WA := TActivatorUtilities.CreateInstance<TWrapperAccessor>(Services);
      try
        // Y no un Assert: con las aserciones desactivadas la llamada desaparece y la demostracion
        // de este sample no demuestra nada, sin que nada lo diga.
        if not WA.ChecWrapperAccess then
          raise Exception.Create('el contenedor no relleno el IWrapper del constructor');
      finally
        WA.Free;
      end;
      Result := TCliExit.Ok;
    end);

  ExitCode := B.Build.Run;
end.
```

## Beneficios del Diseño

1. **Modularidad**: Servicios independientes y reutilizables
2. **Configurabilidad**: Fácil personalización sin recompilar
3. **Mantenibilidad**: Separación clara de responsabilidades
4. **Extensibilidad**: Fácil adición de nuevos servicios

## Ejecución del Ejemplo

1. Compilar y ejecutar la aplicación
2. La aplicación mostrará:
   - Mensajes de saludo personalizados
   - Información de logging
   - Estado de los servicios

## Notas Adicionales

- El ejemplo demuestra las mejores prácticas para aplicaciones de consola
- Se puede extender para incluir más servicios
- Es posible agregar más opciones de configuración
- Se puede implementar manejo de señales del sistema
- El diseño permite fácil migración a servicios de Windows/Linux

## Consideraciones para Producción

1. **Logging**:
   - Configurar niveles apropiados
   - Implementar rotación de logs
   - Considerar integración con sistemas de monitoreo

2. **Configuración**:
   - Usar variables de entorno
   - Implementar validación de configuración
   - Considerar secretos y valores sensibles

3. **Servicios**:
   - Implementar manejo de errores robusto
   - Agregar health checks
   - Considerar tiempos de timeout


