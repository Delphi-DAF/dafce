# TicTacToe — BDD + MVVM Sample

Sample que ilustra **BDD de principio a fin** con MiniSpec y **MVVM con Clean
Architecture**, usando un juego de **3 en Raya** con reglas de movimiento.

## Reglas del juego

- **Tablero**: cuadrícula 3×3.
- **Jugadores**: X (empieza) y O, turnos alternos.

### Fase de colocación

- Cada jugador coloca fichas por turno en casillas vacías.
- Máximo **3 fichas** por jugador.
- Al colocar la 6ª ficha (3 + 3) se pasa automáticamente a la fase de movimiento.

### Fase de movimiento

- Solo se pueden mover **fichas propias**.
- Solo a **casillas adyacentes vacías** (8 direcciones: horizontal, vertical, diagonal).
- No se puede mover a casillas ocupadas ni a casillas no adyacentes.

### Victoria

- Gana quien forme una **línea de 3 fichas propias** (horizontal, vertical o diagonal).
- La victoria puede ocurrir tanto en fase de colocación como de movimiento.
- Una vez hay ganador, no se aceptan más jugadas.

## Arquitectura (Clean Architecture + MVVM)

```
Domain/          → Entidades y reglas de negocio (TTicTacToeGame)
Presentation/    → ViewModel + ViewPort (IGameViewPort, TGameViewModel)
UI/              → VCL Form (TMainForm implementa IGameViewPort)
Specs/           → BDD: World, Step bindings, Features
```

**Dependencias (inside-out):**
- `Domain` no depende de nada externo
- `Presentation` depende de `Domain`
- `UI` depende solo de `Presentation` (no de `Domain`)
- `Specs` operan a través de `Presentation` (E2E por ViewModel)

## Proyectos

| Proyecto | Tipo | Descripción |
|----------|------|-------------|
| `TicTacToeSpecs.dproj` | Console | Spec runner BDD con MiniSpec |
| `TicTacToeApp.dproj` | VCL App | Aplicación gráfica para jugar manualmente |

## Estructura de archivos

### Domain/

| Archivo | Descripción |
|---------|-------------|
| `TicTacToe.Game.pas` | `TTicTacToeGame`, `TPlayer`, `TPosition`, `TGamePhase`, `TGameStatus`, `EInvalidMove` |

### Presentation/

| Archivo | Descripción |
|---------|-------------|
| `TicTacToe.ViewPort.pas` | `IGameViewPort` — interfaz Humble Object para la vista |
| `TicTacToe.ViewModel.pas` | `TGameViewModel` — comandos (PlacePiece, MovePiece, CellClick), queries de dominio y de presentación (CellText, StatusText, PhaseText, IsSelected) |

### UI/

| Archivo | Descripción |
|---------|-------------|
| `TicTacToe.MainForm.pas/dfm` | VCL Form: grid 3×3 de botones, status label, botón Nueva Partida |

### Specs/

| Archivo | Descripción |
|---------|-------------|
| `TicTacToe.SpecHelpers.pas` | `TGameWorld` (contiene ViewModel), constantes X/O/_, helpers |
| `TicTacToe.Placement.Steps.pas` | Step bindings: colocación + steps compartidos |
| `TicTacToe.Movement.Steps.pas` | Step bindings: movimiento |
| `TicTacToe.Victory.Steps.pas` | Step bindings: condiciones de victoria |
| `TicTacToe.UX.Steps.pas` | Step bindings: experiencia de usuario (CellText, StatusText, CellClick) |
| `TicTacToe.Placement.Feat.pas` | Feature: Colocación de fichas (5 specs) |
| `TicTacToe.Movement.Feat.pas` | Feature: Movimiento de fichas (6 specs) |
| `TicTacToe.Victory.Feat.pas` | Feature: Condiciones de victoria (7 specs) |
| `TicTacToe.UX.Feat.pas` | Feature: Experiencia de Usuario (8 specs) |

## Patrones aplicados

- **MVVM**: ViewModel expone estado display-friendly; Form solo renderiza
- **Humble Object**: `IGameViewPort` mantiene la vista desacoplada y testeable
- **Strangler Fig**: ViewModel introdujo gradualmente sin romper tests existentes
- **Observer**: ViewModel notifica ViewPort.Refresh en cada cambio de estado
- **BDD E2E**: Los specs operan a través del ViewModel (mismo camino que la UI)

