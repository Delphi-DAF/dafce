unit KeyedServices.Marks;

{
  El marcador tipado que declararia una APLICACION: una linea sobre el helper generico del
  framework, y con el su enumerado.

  ⚠️ Vive en su propia unit por obligacion, no por orden. Un descendiente de
  DafInjectAttribute<T> declarado en la MISMA unit que la clase que lo usa aborta con un Access
  Violation dentro de GetAttributes -- medido, y bisecado hasta descartar todo lo demas. En la
  suya funciona.
}

interface

uses
  Daf.Extensions.DependencyInjection;

type
  TStorageKind = (skBlob, skFile);

  StorageInjectAttribute = class(DafInjectAttribute<TStorageKind>);

implementation

end.
