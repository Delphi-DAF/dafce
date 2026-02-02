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
// Crear y configurar el host
var Host := THostBuilder.Create
  .ConfigureAppConfiguration(
    procedure(Config: IConfigurationBuilder)
    begin
      Config.AddJsonFile('appsettings.json');
    end)
  .ConfigureServices(
    procedure(Services: IServiceCollection)
    begin
      Services.AddHostedService<TGreeterService>;
    end)
  .Build;

// Ejecutar la aplicación
Host.Run;
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


