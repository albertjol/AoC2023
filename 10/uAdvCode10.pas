unit uAdvCode10;

{$mode delphi}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, EditBtn, StdCtrls
  , Generics.Collections
  ;

type

  { TPoint }

  TPoint = record
    X,Y: Integer;
    class operator = (a, b: TPoint) : Boolean;
    function N(): TPoint;
    function E(): TPoint;
    function S(): TPoint;
    function W(): TPoint;
  end;

  { T2Points }

  T2Points = record
    a, b: TPoint;
    function contains(point: TPoint): Boolean;
  end;

  TState = (
    outside,
    followingLoop,
    unknown,
    crossingIn,
    inside,
    crossingOut
  );

  { TStateMap }
  TStateMap = class(TObjectList<TList<TState>>)
    function IsFilled(): Boolean;
    function At(point: TPoint): TState;
    procedure Print();
  end;

  TMap = TList<TList<Char>>;

  { TFrmAdvCode }

  TFrmAdvCode = class(TForm)
    btn_Start: TButton;
    ed_Answer1: TEdit;
    ed_Answer2: TEdit;
    ed_Filename: TFileNameEdit;
    lbl_Answer2: TLabel;
    lbl_Filename: TLabel;
    lbl_Answer1: TLabel;

    procedure btn_StartClick(Sender: TObject);
    function findStartPoint(map: TMap): TPoint;
    function findExits(point: TPoint; map: TMap) : T2Points;
    procedure FormCreate(Sender: TObject);
    function getChar(point: TPoint; map: TMap): Char;
    procedure part1(map: TMap);
    procedure part2(map: TMap);

  private
    FLoop: TList<TPoint>;
    function DetermineInsideState(const currentState: TState; const nextPoint: TPoint;
      const point: TPoint; const map: TMap; const stateMap: TStateMap): TState;
    function CreateMapFromStringList(const map: TStringList): TMap;
  public

  end;

var
  FrmAdvCode: TFrmAdvCode;

implementation

uses
  FileUtil
  ;

{$R *.lfm}

{ TPoint }

class operator TPoint.=(a, b: TPoint): Boolean;
begin
  Result := ((a.X = b.X) and (a.Y = b.Y));
end;

function TPoint.N(): TPoint;
begin
  Result.X:=X;
  Result.Y:=Y-1;
end;

function TPoint.E(): TPoint;
begin
  Result.X:=X+1;
  Result.Y:=Y;
end;

function TPoint.S(): TPoint;
begin
  Result.X:=X;
  Result.Y:=Y+1;
end;

function TPoint.W(): TPoint;
begin
  Result.X:=X-1;
  Result.Y:=Y;
end;

{ T2Points }

function T2Points.contains(point: TPoint): Boolean;
begin
  Result := (point = a) or (point = b);
end;

{ TStateMap }

function TStateMap.IsFilled(): Boolean;
var
  i, j: Integer;
begin
  Result := True;
  for i := 0 to self.Count - 1 do
  begin
    for j := 0 to self[i].Count - 1 do
    begin
      if (Self[i][j] = unknown) then
      begin
        Result := False;
        Exit;
      end;
    end;
  end;
end;

function TStateMap.At(point: TPoint): TState;
begin
  Result := outside;
  if ((point.Y < 0) or (point.X < 0) or
      (point.Y >= Count - 1) or (point.X >= self[0].Count)) then
    exit;

  Result := self[point.Y][Point.X];
end;

procedure TStateMap.Print();
var
  row: TList<TState>;
  state: TState;
begin
  for row in Self do
  begin
    for state in row do
    begin
      case state of
        followingLoop:
          Write('#');
        crossingIn:
          Write('E');
        crossingOut:
          Write('L');
        inside:
          Write('I');
        outside:
          Write('O');
      end;
    end;
    WriteLn();
  end;
end;

{ TFrmAdvCode }

procedure TFrmAdvCode.btn_StartClick(Sender: TObject);
var
  lines: TStringList;
  map: TMap;
  row: TList<Char>;
begin
  with TStringList.Create() do
  begin
    try
      Add(ed_Filename.Text);
      SaveToFile('settings.txt');
    finally
      Free();
    end;
  end;
  lines := TStringList.Create();
  lines.LoadFromFile(ed_Filename.Text);
  map := CreateMapFromStringList(lines);

  part1(map);
  part2(map);

  for row in map do
  begin
    row.Free();
  end;
  FreeAndNil(map);
  FreeAndNil(lines);
end;

function TFrmAdvCode.findStartPoint(map: TMap): TPoint;
var
  i,j: Integer;
begin
  for i := 0 to map.Count - 1 do
  begin
    for j := 0 to map[i].Count - 1 do
    begin
      if map[i][j] = 'S' then
      begin
        Result.X := j;
        Result.Y := i;
      end;
    end;
  end;
end;

function TFrmAdvCode.findExits(point: TPoint; map: TMap): T2Points;
var
  pipe: Char;
  points: TList<TPoint>;
begin
  pipe := getChar(point, map);
  case pipe of
    '|':
      begin
        Result.a := point.N();
        Result.b := point.S();
      end;
    '-':
      begin
        Result.a := point.W();
        Result.b := point.E();
      end;
    'L':
      begin
        Result.a := point.N();
        Result.b := point.E();
      end;
    'J':
      begin
        Result.a := point.N();
        Result.b := point.W();
      end;
    '7':
      begin
        Result.a := point.W();
        Result.b := point.S();
      end;
    'F':
      begin
        Result.a := point.E();
        Result.b := point.S();
      end;
    'S':
      begin
        points := TList<TPoint>.Create();
        if((point.Y > 0)                      and findExits(point.N(), map).contains(point)) then points.Add(point.N());
        if((point.X < map[point.Y].Count - 1) and findExits(point.E(), map).contains(point)) then points.Add(point.E());
        if((point.Y < map.Count - 1)          and findExits(point.S(), map).contains(point)) then points.Add(point.S());
        if((point.X > 0)                      and findExits(point.W(), map).contains(point)) then points.Add(point.W());
        Result.a := points[0];
        Result.b := points[1];
        FreeAndNil(points);
      end;
  end;
end;

procedure TFrmAdvCode.FormCreate(Sender: TObject);
begin
  if FileIsInPath('settings.txt', '.') then
  begin
    with TStringList.Create() do
    begin
      try
        LoadFromFile('settings.txt');
        if(Count > 0) then
        begin
          ed_Filename.Text := Strings[0];
        end;
      finally
        Free();
      end;
    end;
  end;
end;

function TFrmAdvCode.getChar(point: TPoint; map: TMap): Char;
begin
  Result := map[point.Y][point.X];
end;

procedure TFrmAdvCode.part1(map: TMap);
var
  startPoint : TPoint;
  count: Integer = 0;
  exits: T2Points;
  prevPoints: T2Points;
  exitsTmp: T2Points;
begin
  startPoint := findStartPoint(map);
  FLoop := TList<TPoint>.Create();
  // Save points in FLoop for part 2
  FLoop.Add(startPoint);
//  WriteLn('s.X: ', startPoint.X, ' s.Y: ', startPoint.Y);

  prevPoints.a := startPoint;
  prevPoints.b := startPoint;

  exits := findExits(startPoint, map);
  FLoop.Add(exits.a);
  FLoop.Add(exits.b);

  while (exits.a <> exits.b) do
  begin
    exitsTmp := findExits(exits.a, map);
    if(exitsTmp.a = prevPoints.a) then
    begin
      prevPoints.a := exits.a;
      exits.a := exitsTmp.b;
    end
    else
    begin
      prevPoints.a := exits.a;
      exits.a := exitsTmp.a;
    end;

    exitsTmp := findExits(exits.b, map);
    if(exitsTmp.a = prevPoints.b) then
    begin
      prevPoints.b := exits.b;
      exits.b := exitsTmp.b;
    end
    else
    begin
      prevPoints.b := exits.b;
      exits.b := exitsTmp.a;
    end;
    Inc(count);
    FLoop.Add(exits.a);
    // Last one is probably doubled
    FLoop.Add(exits.b);
//    WriteLn('a.X: ', exits.a.X, ' a.Y: ', exits.a.Y);
//    WriteLn('b.X: ', exits.b.X, ' b.Y: ', exits.b.Y);
  end;
  ed_Answer1.Text := IntToStr(count + 1);
end;

procedure TFrmAdvCode.part2(map: TMap);
var
  i,j: Integer;
  state: TState;
  count: Integer = 0;
  point: TPoint;
  nextPoint: TPoint;
  stateMap: TStateMap;
begin
  stateMap := TStateMap.Create();
  for i := 0 to map.Count - 1 do
  begin
    stateMap.Add(TList<TState>.Create());
    for j := 0 to map[i].Count - 1 do
    begin
      stateMap[i].Add(unknown);
    end;
  end;
  while(not stateMap.IsFilled()) do
  begin
    for i := 0 to map.Count - 1 do
    begin
      state := outside;
      for j := 0 to map[i].Count - 1 do
      begin
        if(stateMap[i][j] <> unknown) then
        begin
          state := stateMap[i][j];
          continue;
        end;

        point.X := j;
        point.Y := i;
        nextPoint := point.E();
        state := DetermineInsideState(state, nextPoint, point, map, stateMap);
        stateMap[i][j] := state;
      end;
    end;
    for j := 0 to map[0].Count - 1 do
    begin
      state := outside;
      for i := 0 to map.Count - 1 do
      begin
        if(stateMap[i][j] <> unknown) then
        begin
          state := stateMap[i][j];
          continue;
        end;

        point.X := j;
        point.Y := i;
        nextPoint := point.S();
        state := DetermineInsideState(state, nextPoint, point, map, stateMap);
        stateMap[i][j] := state;
      end;
    end;

    for i := map.Count - 1 downto 0 do
    begin
      state := outside;
      for j := map[i].Count - 1 downto 0 do
      begin
        if(stateMap[i][j] <> unknown) then
        begin
          state := stateMap[i][j];
          continue;
        end;

        point.X := j;
        point.Y := i;
        nextPoint := point.W();
        state := DetermineInsideState(state, nextPoint, point, map, stateMap);
        stateMap[i][j] := state;
      end;
    end;
    for j := map[0].Count - 1 downto 0 do
    begin
      state := outside;
      for i := map.Count - 1 downto 0 do
      begin
        if(stateMap[i][j] <> unknown) then
        begin
          state := stateMap[i][j];
          continue;
        end;

        point.X := j;
        point.Y := i;
        nextPoint := point.N();
        state := DetermineInsideState(state, nextPoint, point, map, stateMap);
        stateMap[i][j] := state;
      end;
    end;
  end;
  for i := 0 to stateMap.Count - 1 do
  begin
    for j := 0 to stateMap[i].Count - 1 do
    begin
      if(stateMap[i][j] = inside) then
        Inc(count);
    end;
  end;
  stateMap.Print();
  ed_Answer2.Text := IntToStr(count);
end;


function TFrmAdvCode.DetermineInsideState(const currentState: TState;
  const nextPoint: TPoint; const point: TPoint; const map: TMap;
  const stateMap: TStateMap): TState;
begin
  Result := currentState;
  case currentState of
    outside:
      begin
        if (FLoop.Contains(point)) then
        begin
          if(findExits(point, map).contains(nextPoint)) then
            Result := followingLoop
          else
            Result := crossingIn;
        end;
      end;
    followingLoop:
      begin
        if(findExits(point, map).contains(nextPoint)) then
          Result := followingLoop
        else
        begin
          // We're going in or out...
          // We're in if an adjacent pixel is in
          if     (stateMap.At(nextPoint.N()) = inside) then
            Result := crossingIn
          else if(stateMap.At(nextPoint.N()) = outside) then
            Result := crossingOut
          else if(stateMap.At(nextPoint.E()) = inside) then
            Result := crossingIn
          else if(stateMap.At(nextPoint.E()) = outside) then
            Result := crossingOut
          else if(stateMap.At(nextPoint.S()) = inside) then
            Result := crossingIn
          else if(stateMap.At(nextPoint.S()) = outside) then
            Result := crossingOut
          else if(stateMap.At(nextPoint.W()) = inside) then
            Result := crossingIn
          else if(stateMap.At(nextPoint.W()) = outside) then
            Result := crossingOut
          else
            Result := unknown;
        end;
      end;
    unknown:
      begin
        // state does never change...
      end;
    crossingIn:
      begin
      if (not FLoop.Contains(point)) then
      begin
        Result := inside;
      end
      else
        if(findExits(point, map).contains(nextPoint)) then
          Result := followingLoop
        else
          Result := crossingOut;
      end;
    inside:
      begin
        if (FLoop.Contains(point)) then
        begin
          if(findExits(point, map).contains(nextPoint)) then
            Result := followingLoop
          else
            Result := crossingOut;
        end;
      end;
    crossingOut:
      begin
      if (FLoop.Contains(point)) then
      begin
        if(findExits(point, map).contains(nextPoint)) then
          Result := followingLoop
        else
          Result := crossingIn ;
      end
      else
      begin
        Result := outside;
      end;
    end;
  end;
end;

function TFrmAdvCode.CreateMapFromStringList(const map: TStringList): TMap;
var
  i, j: Integer;
  k: Char;
begin
  Result := TMap.Create();
  for i := 0 to map.Count - 1 do
  begin
    Result.Add(TList<Char>.Create());
    for j := 1 to map[i].Length do
    begin
      k := map[i][j];
      Result[i].Add(k);
    end;
  end;
end;

end.

