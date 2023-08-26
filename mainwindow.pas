{$codepage utf8}
unit MainWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ComCtrls, ExtCtrls, Menus, DateUtils, openaiwindow;

type

  { TSearchWindow }

  TSearchWindow = class(TForm)
    BitBtn1: TBitBtn;
    Label1: TLabel;
    MainMenu1: TMainMenu;
    Memo: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    PageControl1: TPageControl;
    Path: TEdit;
    ProgressBar1: TProgressBar;
    SelectLogDirDialog: TSelectDirectoryDialog;
    Start: TButton;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    useAiOut: TCheckBox;
    useAiTask: TCheckBox;
    procedure BitBtn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure StartClick(Sender: TObject);
  private

  public

  end;

  TMyThread = class(TThread)
  protected
    procedure Execute; override;
    destructor Destroy; override;
    function ParseLogFile(logFile: string): TStringList;
  public
    FLogMessage: string;
    FMaxCount: Integer;
    Directory: string;
    procedure UpdateMemo;
    procedure OnExitThread;
    function EscapeCSVValue(const Value: string): string;
    procedure SetMaxProgress;
    procedure UpdateProgress;
    constructor Create(CreateSuspended: Boolean; const Dir: string);
  end;

var
  SearchWindow: TSearchWindow;

implementation

{$R *.lfm}

{ TSearchWindow }

procedure TMyThread.SetMaxProgress;
begin
  SearchWindow.ProgressBar1.Max := FMaxCount;
  SearchWindow.ProgressBar1.Position := 0;
end;

procedure TMyThread.UpdateProgress;
begin
  SearchWindow.ProgressBar1.Position := SearchWindow.ProgressBar1.Position + 1;
  Application.ProcessMessages;
end;

function TMyThread.EscapeCSVValue(const Value: string): string;
begin
  Result := StringReplace(Value, '"', '""', [rfReplaceAll]);
end;

procedure TMyThread.UpdateMemo;
begin
  SearchWindow.Memo.Append(FLogMessage);
end;

function TMyThread.ParseLogFile(logFile: string): TStringList;
var
  lines, data: TStringList;
  i: Integer;
  line, currentInput, currentInputTime, currentTask, hackerNickname, timeStamp,
    resultStr: string;
begin
  Result := TStringList.Create;
  lines := TStringList.Create;
  data := TStringList.Create;
  currentInput := '';
  currentInputTime := '';
  currentTask := '';
  hackerNickname := '';
  lines.LoadFromFile(logFile);
  FLogMessage := 'Парсинг файла: ' + logFile;
  Synchronize(@UpdateMemo);
  i := 0;
  while i < lines.Count do
  begin
    line := lines[i];
    if Pos('[input]', line) > 0 then
    begin
      currentInputTime := Copy(line, 1, Pos(' UTC', line) - 1);
      currentInput := Trim(Copy(line, Pos(']', line) + 1, MaxInt));
      if Pos(' ', currentInput) > 0 then
      begin
        hackerNickname := Copy(currentInput, 1, Pos(' ', currentInput) - 1);
        currentInput := Copy(currentInput, Pos(' ', currentInput) + 1, MaxInt);
      end;
      Inc(i);
    end
    else if Pos('[task]', line) > 0 then
    begin
      currentTask := Trim(Copy(line, Pos(']', line) + 1, MaxInt));
      if (i + 1 < lines.Count) and (Pos('[output]', lines[i + 1]) = 0) and
         (Pos('[error]', lines[i + 1]) = 0) then
      begin
        Result.Add(Format('"%s","%s","%s","%s","",""', [
        EscapeCSVValue(hackerNickname),
        currentInputTime,
        EscapeCSVValue(currentInput),
        EscapeCSVValue(currentTask)]));
        Inc(i);
      end
      else
      begin
        Inc(i);
        Continue;
      end;
    end
    else if (Pos('[output]', line) > 0) or (Pos('[error]', line) > 0) then
    begin
      timeStamp := Copy(line, 1, Pos(' UTC', line) - 1);
      data.Clear;
      Inc(i);
      while (i < lines.Count)
            and (Pos('[input]', lines[i]) = 0)
            and (Pos('[task]', lines[i]) = 0)
            and (Pos('[output]', lines[i]) = 0)
            and (Pos('[error]', lines[i]) = 0)
            and (Pos('[checkin]', lines[i]) = 0) do
      begin
        data.Add(Trim(lines[i]));
        Inc(i);
      end;
      resultStr := StringReplace(data.Text, #13#10, ' ', [rfReplaceAll]);
        Result.Add(Format('"%s","%s","%s","%s","%s","%s"', [
        EscapeCSVValue(hackerNickname),
        currentInputTime,
        EscapeCSVValue(currentInput),
        EscapeCSVValue(currentTask),
        timeStamp,
        EscapeCSVValue(resultStr)]));
    end
    else
      Inc(i);
    end;

    lines.Free;
    data.Free;

end;

procedure TMyThread.OnExitThread;
begin
  SearchWindow.ProgressBar1.Position := 0;
  SearchWindow.Memo.Append('Парсинг завершен!');
  SearchWindow.Start.Enabled := True;
end;

destructor TMyThread.Destroy;
begin
  Synchronize(@OnExitThread);
end;

procedure TMyThread.Execute;
var
  FoundFiles, parsedData, CSVOutput: TStringList;
  i: Integer;
  CSVFileName: string;
  procedure SearchFiles(const ADir: string);
  var
    LocalSearchRec: TSearchRec;
  begin
    if FindFirst(IncludeTrailingPathDelimiter(ADir) + '*.*',
       faAnyFile, LocalSearchRec) = 0 then
    try
      repeat
        if (LocalSearchRec.Attr and faDirectory) <> 0 then
        begin
          if (LocalSearchRec.Name <> '.') and (LocalSearchRec.Name <> '..') then
            SearchFiles(IncludeTrailingPathDelimiter(ADir) + LocalSearchRec.Name);
        end
        else if (Pos('beacon', LocalSearchRec.Name) = 1) then
        begin
          FoundFiles.Add(IncludeTrailingPathDelimiter(ADir) + LocalSearchRec.Name);
        end;
      until FindNext(LocalSearchRec) <> 0;
    finally
      FindClose(LocalSearchRec);
    end;
  end;
begin
  FoundFiles := TStringList.Create;
  CSVOutput := TStringList.Create;
  try
    SearchFiles(Directory);
    FLogMessage := 'Всего файлов найдено: ' + FoundFiles.Count.toString;
    FMaxCount := FoundFiles.Count;
    Synchronize(@SetMaxProgress);
    Synchronize(@UpdateMemo);
    CSVOutput.Add('HackerNickname,InputTime,InputCommand,Task,Output/ErrorTime,Output/ErrorResult');
    for i := 0 to FoundFiles.Count - 1 do
    begin
      parsedData := ParseLogFile(FoundFiles[i]);
      Synchronize(@UpdateProgress);
      CSVOutput.AddStrings(parsedData);
      parsedData.Free;
    end;
    CSVFileName := IncludeTrailingPathDelimiter(Directory) + 'parsed_logs.csv';
    CSVOutput.SaveToFile(CSVFileName);
  finally
    FoundFiles.Free;
    CSVOutput.Free;
  end;
end;

constructor TMyThread.Create(CreateSuspended: Boolean; const Dir: string);
begin
  inherited Create(CreateSuspended);
  Directory := Dir;
end;

procedure TSearchWindow.FormCreate(Sender: TObject);
begin
  Form1 := TForm1.Create(SearchWindow);
  SelectLogDirDialog.InitialDir := GetUserDir + 'Desktop';
  useAiTask.Enabled:=False;
  useAiOut.Enabled:=False;
end;

procedure TSearchWindow.FormDestroy(Sender: TObject);
begin

end;

procedure TSearchWindow.MenuItem2Click(Sender: TObject);
begin
  Form1.Position := poOwnerFormCenter;
  Form1.ShowModal;
end;

procedure TSearchWindow.StartClick(Sender: TObject);
begin
  if Trim(Path.Text) <> '' then
  begin
    Start.Enabled := False;
    with TMyThread.Create(True, Path.Text) do
    begin
      FreeOnTerminate := True;
      Start;
    end;
  end;
end;

procedure TSearchWindow.BitBtn1Click(Sender: TObject);
begin
  if SelectLogDirDialog.Execute then
  begin
       Path.Text := SelectLogDirDialog.FileName
  end;
end;

end.
