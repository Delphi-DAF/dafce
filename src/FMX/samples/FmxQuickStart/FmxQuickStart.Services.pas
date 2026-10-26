unit FmxQuickStart.Services;

{
  El servicio que el formulario consume. Vive en una unit y no en el .dpr porque el
  contenedor lo resuelve por RTTI, y un tipo declarado en el .dpr no esta en la seccion
  interface de ninguna unit: resolverlo aborta en ejecucion con ENonPublicType.
}

interface

uses
  System.SysUtils;

type
  IGreeter = interface(IInvokable)
    ['{2F7A9C41-58D3-4E06-B19F-C4A2D6570E38}']
    function Greet(const Name: string): string;
    function Count: Integer;
  end;

  TGreeter = class(TInterfacedObject, IGreeter)
  strict private
    FCount: Integer;
  public
    function Greet(const Name: string): string;
    function Count: Integer;
  end;

implementation

{ TGreeter }

function TGreeter.Greet(const Name: string): string;
begin
  Inc(FCount);
  if Name.Trim.IsEmpty then
    Result := 'Hola, quienquiera que seas.'
  else
    Result := Format('Hola, %s.', [Name.Trim]);
end;

function TGreeter.Count: Integer;
begin
  Result := FCount;
end;

end.
