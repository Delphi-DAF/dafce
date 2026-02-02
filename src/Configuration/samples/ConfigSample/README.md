# Ejemplo de Configuración en DAF

Este ejemplo demuestra el uso del sistema de configuración de DAF (Delphi Application Framework), mostrando diferentes formas de manejar la configuración de una aplicación.

## Estructura del Proyecto

El proyecto está organizado en varios archivos que muestran diferentes enfoques de configuración:

- `ConfigExample.dpr`: Punto de entrada de la aplicación
- `FileConfig.pas`: Implementación de configuración basada en archivos
- `InMemoryConfig.pas`: Implementación de configuración en memoria
- `Files/`: Directorio con archivos de configuración de ejemplo

## Patrones Utilizados

### 1. Configuration Pattern
El patrón de configuración se utiliza para:
- Separar la configuración del código
- Proporcionar diferentes fuentes de configuración
- Permitir la modificación de la configuración sin recompilar

### 2. Strategy Pattern
Se utiliza para:
- Intercambiar diferentes implementaciones de configuración
- Mantener una interfaz consistente para acceder a la configuración
- Facilitar la adición de nuevas fuentes de configuración

## Componentes Principales

### Configuración en Memoria
```pascal
TInMemoryConfig = class(TInterfacedObject, IConfiguration)
  // Implementación de configuración que mantiene los valores en memoria
end;
```

### Configuración en Archivo
```pascal
TFileConfig = class(TInterfacedObject, IConfiguration)
  // Implementación de configuración que lee/escribe en archivos
end;
```

## Características del Sistema de Configuración

1. **Múltiples Fuentes**:
   - Configuración en memoria
   - Configuración en archivos
   - Soporte para diferentes formatos (JSON, INI, etc.)

2. **Jerarquía de Configuración**:
   - Valores por defecto
   - Configuración por entorno
   - Configuración específica de usuario

3. **Tipos de Datos Soportados**:
   - Strings
   - Números
   - Booleanos
   - Arrays
   - Objetos anidados

## Ejemplo de Uso

```pascal
// Crear una configuración en memoria
var Config := TInMemoryConfig.Create;

// Establecer valores
Config.SetValue('Database.ConnectionString', 'Server=localhost;Database=test');
Config.SetValue('Logging.Level', 'Debug');

// Leer valores
var ConnectionString := Config.GetValue<string>('Database.ConnectionString');
var LogLevel := Config.GetValue<string>('Logging.Level');
```

## Beneficios del Diseño

1. **Flexibilidad**: Fácil cambio entre diferentes fuentes de configuración
2. **Mantenibilidad**: Separación clara entre código y configuración
3. **Extensibilidad**: Fácil adición de nuevas fuentes de configuración
4. **Testabilidad**: Configuración en memoria facilita las pruebas

## Ejecución del Ejemplo

1. Compilar y ejecutar la aplicación
2. El ejemplo mostrará:
   - Carga de configuración desde diferentes fuentes
   - Lectura de valores de configuración
   - Modificación de valores
   - Persistencia de cambios

## Notas Adicionales

- El ejemplo incluye implementaciones básicas para demostrar el concepto
- En una aplicación real, se podrían agregar más fuentes de configuración
- Se pueden implementar validaciones adicionales de configuración
- Es posible agregar soporte para encriptación de valores sensibles
- Se puede extender para soportar hot-reloading de configuración



