# Ejemplo de Mediator con VCL

Este ejemplo demuestra el uso del patrón Mediator en una aplicación VCL utilizando DAF (Delphi Application Framework). El ejemplo implementa un CRUD simple de clientes utilizando el patrón CQRS (Command Query Responsibility Segregation) y el patrón Mediator.

## Estructura del Proyecto

El proyecto está organizado en varios archivos que separan las diferentes responsabilidades:

- `MediatRSample.dpr`: Punto de entrada de la aplicación y configuración de dependencias
- `MediatRSample.Requests.pas`: Define los comandos, consultas y eventos del sistema
- `MediatRSample.Handlers.pas`: Implementa los manejadores de comandos y consultas
- `MediatRSample.MainForm.pas`: Interfaz de usuario y coordinación

## Patrones Utilizados

### 1. Mediator Pattern
El patrón Mediator se utiliza para desacoplar los componentes de la aplicación. En este ejemplo:
- Los formularios no conocen la implementación de los comandos/queries
- Los handlers no conocen cómo se presentan los datos
- La comunicación se realiza a través de mensajes (comandos/queries)

### 2. CQRS (Command Query Responsibility Segregation)
El ejemplo separa las operaciones en:
- **Comandos**: Modifican el estado (ej: `TAddCustomerCommand`)
- **Queries**: Consultan el estado (ej: `TCustomerQuery`)

### 3. Dependency Injection
Se utiliza el contenedor de dependencias de DAF para:
- Registrar y resolver servicios
- Inyectar dependencias en los handlers
- Gestionar el ciclo de vida de los objetos

## Componentes Principales

### Comandos y Eventos
```pascal
TAddCustomerCommand = class(TCommand)
  // Comando para agregar un nuevo cliente
end;

TCustomerAddedEvent = class(TEvent)
  // Evento publicado cuando se agrega un cliente
end;
```

### Queries
```pascal
TCustomerQuery = class(TQuery)
  // Query para obtener la lista de clientes
end;

TCustomerResult = class
  // Resultado de la query con la lista de clientes
end;
```

### Handlers
```pascal
TAddCustomerCommandHandler = class(TInterfacedObject, ICommandHandler<TAddCustomerCommand>)
  // Maneja la lógica de agregar clientes
end;

TCustomerQueryHandler = class(TInterfacedObject, IQueryHandler<TCustomerQuery, TCustomerResult>)
  // Maneja la lógica de consultar clientes
end;
```

## Configuración de Dependencias

La configuración de dependencias se realiza en el archivo principal:

```pascal
// Configurar el contenedor de servicios
ServiceCollection := TServiceCollection.Create;

// Registrar Mediator y sus handlers
ServiceCollection.AddMediatR;
ServiceCollection.AddMediatRClasses(System.Rtti.TRttiContext.Create.GetPackage(MediatRSample));

// Registrar el servicio compartido de clientes
ServiceCollection.AddSingleton<TObjectList<TCustomer>>;

// Construir el proveedor de servicios
ServiceProvider := ServiceCollection.BuildServiceProvider;
```

## Flujo de la Aplicación

1. **Inicio de la Aplicación**:
   - Se configura el contenedor de dependencias
   - Se registran los servicios y handlers
   - Se crea el formulario principal

2. **Agregar Cliente**:
   - El usuario ingresa el nombre del cliente
   - Se crea un `TAddCustomerCommand`
   - El Mediator envía el comando al handler correspondiente
   - El handler procesa el comando y publica un evento
   - La UI se actualiza con la nueva lista de clientes

3. **Consultar Clientes**:
   - Se crea un `TCustomerQuery`
   - El Mediator envía la query al handler correspondiente
   - El handler retorna la lista de clientes
   - La UI muestra los resultados

## Beneficios del Diseño

1. **Desacoplamiento**: Los componentes se comunican a través de mensajes, reduciendo el acoplamiento.
2. **Testabilidad**: Los handlers pueden probarse de forma aislada.
3. **Mantenibilidad**: La separación de responsabilidades facilita el mantenimiento.
4. **Escalabilidad**: Es fácil agregar nuevos comandos/queries sin modificar el código existente.

## Ejecución del Ejemplo

1. Compilar y ejecutar la aplicación
2. Ingresar nombres de clientes en el campo de texto
3. Hacer clic en "Agregar Cliente" para agregar nuevos clientes
4. La lista se actualizará automáticamente mostrando los clientes agregados

## Notas Adicionales

- El ejemplo utiliza un almacenamiento en memoria (TObjectList) para simplicidad
- En una aplicación real, se podría agregar persistencia de datos
- Se pueden agregar más comandos/queries siguiendo el mismo patrón
- Los eventos permiten notificar cambios a múltiples componentes 