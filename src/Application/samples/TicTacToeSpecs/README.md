# TicTacToe Specs

Sample que ilustra BDD de principio a fin con MiniSpec, usando un juego de
**3 en Raya** con reglas de movimiento.

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

## Estructura

| Archivo | Descripción |
|---------|-------------|
| `TicTacToe.Game.pas` | Modelo de dominio |
| `TicTacToe.SpecHelpers.pas` | World (`TGameWorld`), constantes y helpers |
| `TicTacToe.Placement.Steps.pas` | Step bindings de colocación |
| `TicTacToe.Movement.Steps.pas` | Step bindings de movimiento |
| `TicTacToe.Victory.Steps.pas` | Step bindings de victoria |
| `TicTacToe.Placement.Feat.pas` | Feature: Colocación de fichas |
| `TicTacToe.Movement.Feat.pas` | Feature: Movimiento de fichas |
| `TicTacToe.Victory.Feat.pas` | Feature: Condiciones de victoria |
