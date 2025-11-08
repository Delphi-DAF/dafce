unit MiniSpec.DirectoryWatcher;

interface

uses
  System.Classes, System.SysUtils, Winapi.Windows, System.IOUtils;

type
  PFILE_NOTIFY_INFORMATION = ^FILE_NOTIFY_INFORMATION;
  FILE_NOTIFY_INFORMATION = record
    NextEntryOffset: DWORD;
    Action: DWORD;
    FileNameLength: DWORD;
    FileName: array [0..0] of WideChar;
  end;

const
  FILE_ACTION_ADDED              = $00000001;
  FILE_ACTION_REMOVED            = $00000002;
  FILE_ACTION_MODIFIED           = $00000003;
  FILE_ACTION_RENAMED_OLD_NAME   = $00000004;
  FILE_ACTION_RENAMED_NEW_NAME   = $00000005;

type
  TFileAction = (faAdded, faRemoved, faModified, faRenamedOldName, faRenamedNewName);
  TFileChangeEvent = reference to procedure(Sender: TObject; const FileName: string; Action: TFileAction);

  TDirectoryWatcher = class(TThread)
  private
    FStopEvent: THandle;
    FDirectory: string;
    FPattern: string;
    FOnChange: TFileChangeEvent;
    FHandle: THandle;
    FBuffer: array[0..4095] of Byte;
    FWatchSubtree: Boolean;
    procedure Notify(const FullFileName: string; Action: TFileAction);
    function MatchPattern(const FullFileName: string): Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(const Directory, Pattern: string; OnChange: TFileChangeEvent;
      WatchSubtree: Boolean = False);
    procedure StopWatching;
    destructor Destroy; override;
  end;

implementation

constructor TDirectoryWatcher.Create(const Directory, Pattern: string; OnChange: TFileChangeEvent; WatchSubtree: Boolean = False);
begin
  inherited Create(True);
  FStopEvent := CreateEvent(nil, True, False, nil);
  FDirectory := IncludeTrailingPathDelimiter(Directory);
  FPattern := Pattern;
  FOnChange := OnChange;
  FWatchSubtree := WatchSubtree;
  FHandle := CreateFileW(PChar(FDirectory), FILE_LIST_DIRECTORY,
  FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE, nil,
  OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS or FILE_FLAG_OVERLAPPED, 0);
end;

destructor TDirectoryWatcher.Destroy;
begin
  StopWatching;
  WaitFor;
  if FStopEvent <> 0 then
    CloseHandle(FStopEvent);
  inherited;  inherited;
end;

function TDirectoryWatcher.MatchPattern(const FullFileName: string): Boolean;
begin
  Result := TPath.MatchesPattern(TPath.GetFileName(FullFileName), FPattern, False);
end;

procedure TDirectoryWatcher.StopWatching;
begin
  Terminate;
  if FStopEvent <> 0 then
    SetEvent(FStopEvent);
end;

procedure TDirectoryWatcher.Notify(const FullFileName: string; Action: TFileAction);
begin
  if Assigned(FOnChange) and MatchPattern(FullFileName) then
    TThread.Queue(nil, procedure
    begin
      FOnChange(Self, FullFileName, Action);
    end);
end;

procedure TDirectoryWatcher.Execute;
var
  BytesReturned: DWORD;
  NotifyInfo: PFILE_NOTIFY_INFORMATION;
  Offset: Integer;
  Action: TFileAction;
  FileName: string;
  FileNameLen: Integer;
  ovl: TOverlapped;
  hEvents: array[0..1] of THandle;
  wres: DWORD;
begin
  if FHandle = INVALID_HANDLE_VALUE then
    Exit;

  ZeroMemory(@ovl, SizeOf(ovl));
  ovl.hEvent := CreateEvent(nil, True, False, nil);
  hEvents[0] := ovl.hEvent;
  hEvents[1] := FStopEvent;

  while not Terminated do
  begin
    ResetEvent(ovl.hEvent);
    if not ReadDirectoryChangesW(
      FHandle, @FBuffer[0], SizeOf(FBuffer), FWatchSubtree,
      FILE_NOTIFY_CHANGE_FILE_NAME or FILE_NOTIFY_CHANGE_LAST_WRITE,
      @BytesReturned, @ovl, nil) then
      Break;

    wres := WaitForMultipleObjects(2, @hEvents, False, INFINITE);
    if wres = WAIT_OBJECT_0 + 1 then
      Break;

    if wres = WAIT_OBJECT_0 then
    begin
      if GetOverlappedResult(FHandle, ovl, BytesReturned, False) then
      begin
        Offset := 0;
        repeat
          NotifyInfo := PFILE_NOTIFY_INFORMATION(@FBuffer[Offset]);
          FileNameLen := NotifyInfo.FileNameLength div SizeOf(WideChar);
          SetString(FileName, PWideChar(@NotifyInfo.FileName), FileNameLen);

          case NotifyInfo.Action of
            FILE_ACTION_ADDED:            Action := faAdded;
            FILE_ACTION_REMOVED:          Action := faRemoved;
            FILE_ACTION_MODIFIED:         Action := faModified;
            FILE_ACTION_RENAMED_OLD_NAME: Action := faRenamedOldName;
            FILE_ACTION_RENAMED_NEW_NAME: Action := faRenamedNewName;
          else
            Action := faModified;
          end;
          Notify(FDirectory + FileName, Action);

          if NotifyInfo.NextEntryOffset = 0 then
            Break;
          Inc(Offset, NotifyInfo.NextEntryOffset);
        until False;
      end;
    end;
  end;

  CloseHandle(ovl.hEvent);
end;

end.

