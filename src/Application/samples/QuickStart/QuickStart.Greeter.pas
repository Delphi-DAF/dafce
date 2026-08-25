unit QuickStart.Greeter;

// El servicio va en una unit y no en el .dpr por obligacion del contenedor, no por gusto: un
// tipo declarado en el fichero de programa no esta en la seccion interface de ninguna unit, y
// resolverlo aborta en tiempo de ejecucion con ENonPublicType.

interface

type
  IGreeter = interface(IInvokable)
    ['{2B9E4D07-5C31-4A68-8F52-71D3A6C0B94E}']
    function Greet: string;
  end;

  TGreeter = class(TInterfacedObject, IGreeter)
  public
    function Greet: string;
  end;

implementation

function TGreeter.Greet: string;
begin
  Result := 'Hola desde DAF';
end;

end.
