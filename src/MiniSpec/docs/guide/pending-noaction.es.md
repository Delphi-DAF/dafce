# Pending y NoAction

**🌍 Idioma: [English](pending-noaction.md) | Español**

[← Volver a la Guía](../GUIDE.es.md)

---

MiniSpec proporciona dos formas de marcar steps que no tienen código ejecutable:

| Método | Semántica | Resultado | Uso típico |
|--------|-----------|-----------|------------|
| `.Pending` | "Pendiente de implementar" | Skip + detiene escenario | Work in progress |
| `.NoAction` | "Sin acción requerida" | Pass + continúa escenario | Steps puramente descriptivos |

## Pending

Indica que un step está pendiente de implementar. El escenario se marca como Skip y **no se ejecutan los steps siguientes**:

```pascal
.Scenario('Feature en desarrollo')
  .Given('una precondición', procedure(Ctx: TMyWorld) begin end)
  .When('algo pendiente de implementar')
    .Pending                          // Escenario Skip, no sigue
  .&Then('nunca se ejecuta', procedure(Ctx: TMyWorld)
    begin
      // Este código no se ejecuta
    end)
```

## NoAction

Marca un step como puramente descriptivo. Pasa automáticamente y **los steps siguientes sí se
ejecutan**:

```pascal
.Scenario('Con un contexto descriptivo')
  .Given('el sistema está configurado')  // el entorno ya lo da
    .NoAction
  .When('el usuario hace login', procedure(Ctx: TMyWorld)
    begin
      Ctx.DoLogin;  // este código SÍ se ejecuta
    end)
  .&Then('accede al dashboard', procedure(Ctx: TMyWorld)
    begin
      Expect(Ctx.IsLoggedIn).ToBeTrue;  // también se ejecuta
    end)
```

## El último recurso

Un escenario afirma el comportamiento de **un evento**, repartido en las tres fases: el `Given` deja
el estado que el evento necesita, el `When` ejecuta el evento y el `Then` observa lo que queda. Un
step sin cuerpo dice que esa pieza falta, y el escenario deja de decir qué valida.

No está prohibido —la marca existe por algo— pero **su uso legítimo es raro, y rarísimo en un `When`
y en un `Then`**, así que se justifica al usarla en vez de darse por buena.

**La regla que decide: un paso que no tiene nada que *ejecutar* casi siempre tiene algo que
*comprobar*.** Esa es su forma de check, y es lo que deja `.NoAction` para el residuo de verdad.

| Fase | Qué hacer cuando no hay nada que ejecutar |
|---|---|
| `Given` | afirmar que el entorno es el que supones |
| `When` | en una **acción implícita**, esperar a que haya ocurrido y afirmar que ocurrió |
| `Then` | aquí no cabe nada: un escenario que no observa nada no afirma nada |

Así que antes de escribirlo, dos preguntas: **¿hay algo que ejecutar aquí?** y **¿hay algo que
comprobar aquí?** `.NoAction` es la respuesta cuando las dos son que no.

### La acción implícita

Hay eventos que produce el sistema y no el escenario: una tarea en segundo plano que corre sola, un
temporizador, algo que ya ocurrió mientras se inicializaba una unit. El `When` nombra el evento
aunque el test no lo haya causado.

```pascal
.When('acción implícita').NoAction
```

**Dos preguntas antes de aceptarlo**, porque la mayoría de los steps que parecen implícitos no lo son:

1. **¿Puedo dispararlo?** Entonces dispáralo en el `When`, y este no es el caso.
2. **¿Puedo esperarlo?** Entonces la espera es el cuerpo del step:

```pascal
.When('la tarea en segundo plano ya corrió',
  procedure(W: TMyWorld)
  begin
    Expect(W.WaitForTask(5000)).ToBeTrue;
  end)
```

⚠️ **Un `When` que espera quita la carrera.** Sin él, el `Then` puede correr antes de que la tarea
termine, y entonces el escenario pasa o falla según lo cargada que esté la máquina — que es
fragilidad, no una cuestión de forma.

⚠️ **En todo lo demás, un `When` sin acción imprime `✓` igual**, así que el informe declara un evento
que no ocurrió y se lee como cubierto. Si el escenario no tiene evento ninguno, no tiene
comportamiento que validar: **omitir el `When` en vez de desactivarlo es la misma situación con otro
rótulo.** Donde suele aparecer el evento es en el `Given`, que lo ejecutó antes de tiempo, o dentro
del `Then`, que lo ejecuta mientras comprueba.

⚠️ **Nada impide ponerlo en un `Then`, y nada avisa.** El step pasa a ser un cuerpo que no hace nada
y que pasa, así que el escenario se queda con un `Then` que no verifica nada y sigue en verde — que
es peor que un fallo, porque parece cobertura.

```pascal
.&Then('verificación').NoAction  // compila, se ejecuta, no verifica nada
```

## El `Given` que no tiene nada que construir

El caso legítimo existe y es estrecho: el entorno ya tiene el estado, así que no hay nada que
preparar. **Aun así, comprobar es mejor que no hacer nada** — afirma que el entorno es el que supones:

```pascal
.Given('el sistema está configurado',
  procedure(W: TMyWorld)
  begin
    Expect(W.Environment.IsReady).ToBeTrue;
  end)
```

Un `Given` que afirma su punto de partida convierte un fallo de entorno en un rojo que dice *el
entorno no era el que suponías*, en vez de un `Then` que falla y te manda a buscar donde no es.

## Dónde cae

`.NoAction` sustituye el cuerpo del **último step declarado**, sea del tipo que sea:

```pascal
.Given('contexto descriptivo').NoAction
```

---

[← Vocabulario Gherkin](gherkin-vocabulary.es.md) | [Siguiente: Hooks →](hooks.es.md)
