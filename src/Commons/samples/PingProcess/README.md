# Ejemplo de Manejo de Procesos con TSystemProcess en DAF

Este ejemplo demuestra el uso de `TSystemProcess` de DAF (Delphi Application Framework) para manejar procesos del sistema de manera moderna y asíncrona, utilizando el patrón Builder y callbacks para el control de eventos.

## Estructura del Proyecto

El proyecto demuestra el uso de `TSystemProcess` a través de un ejemplo práctico de ejecución de ping:

- `PingProcess.dpr`: Aplicación principal que demuestra el uso de `TSystemProcess.Builder` para ejecutar y controlar un proceso de ping
- `PingProcess.dproj`: Archivo de proyecto Delphi
- `PingProcess.res`: Recursos de la aplicación

## Características Principales

### 1. Patrón Builder
`TSystemProcess.Builder` proporciona una interfaz fluida para configurar procesos:
```pascal
var Process := TSystemProcess.Builder
  .Command('cmd.exe')
  .CmdArgs(['/k', 'ping', '127.0.0.1'])
  .Timeout(18000)
  .OnStdOut(procedure(Text: string) begin ... end)
  .OnStdErr(procedure(Text: string) begin ... end)
  .OnCompleted(procedure(Result: TProcessResult) begin ... end)
  .Build;
```

### 2. Manejo Asíncrono
- Ejecución no bloqueante con `ExecuteAsync`
- Soporte para cancelación con `CancellationToken`
- Callbacks para diferentes eventos del proceso

### 3. Eventos del Proceso
- `OnStdOut`: Captura de salida estándar
- `OnStdErr`: Captura de errores
- `OnCompleted`: Finalización exitosa
- `OnFailed`: Fallo en la ejecución
- `OnCancelled`: Cancelación del proceso
- `OnKilled`: Proceso terminado forzosamente
- `OnIdle`: Proceso en espera

## Ejemplo de Implementación

```pascal
// Crear y configurar el proceso
var Process := TSystemProcess.Builder
  .Command('cmd.exe')
  .CmdArgs(['/k', 'ping', '127.0.0.1'])
  .Timeout(18000)
  .OnStdOut(procedure(Text: string)
    begin
      Writeln(Text);
    end)
  .OnCompleted(procedure(Result: TProcessResult)
    begin
      Writeln('Status: ', TEnum.ToString(Result.Status));
      Writeln('ExitCode: ', Result.ExitCode);
      Writeln('Duration: ', Result.Duration.Seconds);
    end)
  .Build;

// Ejecutar de forma asíncrona
var token := CreateCancellationTokenSource;
Process.ExecuteAsync(token.token);

// Cancelar después de un tiempo
token.Cancel;
```

## Características del TSystemProcess

### 1. Configuración Flexible
- Comando y argumentos
- Timeout personalizable
- Redirección de entrada/salida
- Opciones de ventana (oculta/visible)
- Directorio de trabajo

### 2. Control de Proceso
- Ejecución asíncrona
- Cancelación controlada
- Monitoreo de estado
- Captura de salida en tiempo real
- Manejo de errores

### 3. Resultados Detallados
- Estado del proceso
- Código de salida
- Último error
- Duración de ejecución
- Salida capturada

## Beneficios del Diseño

1. **Moderno**: API fluida y asíncrona
2. **Seguro**: Manejo automático de recursos
3. **Flexible**: Configuración extensible
4. **Robusto**: Manejo completo de eventos

## Consideraciones para Producción

### 1. Manejo de Errores
- Implementar todos los callbacks de evento
- Validar resultados del proceso
- Manejar timeouts apropiadamente
- Registrar errores y excepciones

### 2. Rendimiento
- Usar timeouts apropiados
- Liberar recursos correctamente
- Manejar cancelaciones
- Monitorear uso de memoria

### 3. Seguridad
- Validar comandos y argumentos
- Usar rutas absolutas cuando sea necesario
- Considerar privilegios de ejecución
- Sanitizar entrada/salida

## Ejemplos de Uso

### Proceso con Timeout
```pascal
var Process := TSystemProcess.Builder
  .Command('ping')
  .CmdArgs(['-n', '1', 'localhost'])
  .Timeout(5000)
  .OnCompleted(procedure(Result: TProcessResult)
    begin
      if Result.Status = TProcessStatus.Completed then
        Writeln('Ping completado en ', Result.Duration.Milliseconds, 'ms')
      else
        Writeln('Error: ', TEnum.ToString(Result.Status));
    end)
  .Build;

Process.ExecuteAsync(nil);
```

### Proceso con Captura de Salida
```pascal
var Output := TStringList.Create;
try
  var Process := TSystemProcess.Builder
    .Command('cmd.exe')
    .CmdArgs(['/c', 'dir'])
    .OnStdOut(procedure(Text: string)
      begin
        Output.Add(Text);
      end)
    .Build;

  Process.ExecuteAsync(nil);
  // Procesar Output después de la ejecución
finally
  Output.Free;
end;
```

## Notas Adicionales

- `TSystemProcess` reemplaza el uso directo de `TProcess`
- Proporciona una API más moderna y segura
- Facilita el manejo asíncrono de procesos
- Integra bien con el resto de DAF
- Soporta patrones modernos de programación

## Mejores Prácticas

1. **Configuración**:
   - Usar el Builder para toda la configuración
   - Establecer timeouts apropiados
   - Implementar todos los callbacks necesarios
   - Validar parámetros antes de ejecutar

2. **Ejecución**:
   - Preferir `ExecuteAsync` sobre `Execute`
   - Usar `CancellationToken` para control
   - Manejar todos los eventos posibles
   - Liberar recursos apropiadamente

3. **Manejo de Errores**:
   - Implementar todos los callbacks de error
   - Validar resultados del proceso
   - Registrar errores detalladamente
   - Manejar excepciones apropiadamente 