unit TicTacToe.ViewPort;

interface

type
  /// <summary>
  /// Abstraction for the game view (Humble Object pattern).
  /// The ViewModel notifies through this interface when state changes.
  /// The VCL Form implements it; specs can use a null or spy implementation.
  /// </summary>
  IGameViewPort = interface
    ['{A7B3C4D5-E6F7-4A8B-9C0D-1E2F3A4B5C6D}']
    /// <summary>Repaint entire view from ViewModel state.</summary>
    procedure Refresh;
  end;

implementation

end.
