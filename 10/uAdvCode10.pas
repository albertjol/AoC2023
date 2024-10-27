unit uAdvCode10;

{$mode objfpc}{$H+}
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
  TStateList = specialize TList<TState>;
  TStateMap = class(specialize TObjectList<TStateList>)
    function IsFilled(): Boolean;
  end;

  { TFrmAdvCode }

  TFrmAdvCode = class(TForm)
    btn_Start: TButton;
    ed_Answer: TEdit;
    ed_Filename: TFileNameEdit;
    lbl_Filename: TLabel;
    lbl_Answer: TLabel;

    procedure btn_StartClick(Sender: TObject);
    function findStartPoint(map: TStringList): TPoint;
    function findExits(point: TPoint; map: TStringList) : T2Points;
    function getChar(point: TPoint; map: TStringList): Char;
    procedure part1(map: TStringList);
    procedure part2(map: TStringList);

  private
    FLoop: specialize TList<TPoint>;
    function DetermineInsideState(const nextPoint: TPoint; const point: TPoint;
      const map: TStringList): TState;
  public

  end;

var
  FrmAdvCode: TFrmAdvCode;

implementation

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
  i: Integer;
begin
  Result := True;
  for (i := 0 to self.Count - 1) do
  begin
    for (j := 0 to self[i].Count - 1) do
    begin
      if (Self[i][j] = unknown) then
      begin
        Result := False;
        Exit;
      end;
    end;
  end;
end;

{ TFrmAdvCode }

procedure TFrmAdvCode.btn_StartClick(Sender: TObject);
var
  lines: TStringList;
begin
  lines := TStringList.Create();
  lines.LoadFromFile(ed_Filename.Text);

  part1(lines);
  part2(lines);

  FreeAndNil(lines);
end;

function TFrmAdvCode.findStartPoint(map: TStringList): TPoint;
var
  i,j: Integer;
begin
  for i := 0 to map.Count - 1 do
  begin
    for j := 1 to map[0].Length do // Strings are 1-based
    begin
      if map[i][j] = 'S' then
      begin
        Result.X := j - 1; // Strings are 1-based...
        Result.Y := i;
      end;
    end;
  end;
end;

function TFrmAdvCode.findExits(point: TPoint; map: TStringList): T2Points;
var
  pipe: Char;
  points: specialize TList<TPoint>;
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
        points := specialize TList<TPoint>.Create();
        if((point.Y > 0)                   and findExits(point.N(), map).contains(point)) then points.Add(point.N());
        if((point.X < map[point.Y].Length) and findExits(point.E(), map).contains(point)) then points.Add(point.E());
        if((point.Y < map.Count - 1)       and findExits(point.S(), map).contains(point)) then points.Add(point.S());
        if((point.X > 0)                   and findExits(point.W(), map).contains(point)) then points.Add(point.W());
        Result.a := points[0];
        Result.b := points[1];
        FreeAndNil(points);
      end;
  end;
end;

function TFrmAdvCode.getChar(point: TPoint; map: TStringList): Char;
begin
  Result := map[point.Y][point.X + 1];
end;

procedure TFrmAdvCode.part1(map: TStringList);
var
  startPoint : TPoint;
  count: Integer = 0;
  exits: T2Points;
  prevPoints: T2Points;
  exitsTmp: T2Points;
begin
  startPoint := findStartPoint(map);

  // Save points in FLoop for part 2
  FLoop.Add(startPoint);

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
  end;

  ed_Answer.Text := IntToStr(count + 1);
end;

procedure TFrmAdvCode.part2(map: TStringList);
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
    stateMap.Add(specialize TList<TState>.Create());
    for j := 0 to map[i].Length - 1 do
    begin
      stateMap[i].Add(unknown);
    end;
  end;
  while(not stateMap.IsFilled()) do
  begin
    for i := 0 to map.Count - 1 do
    begin
      state := outside;
      for j := 1 to map[i].Length do
      begin
        if(stateMap[i][j-1] <> unknown) then
        begin
          state := stateMap[i][j-1];
          continue;
        end;

        point.X:=j;
        point.Y:=i;
        nextPoint := point.E();
        state:=DetermineInsideState(nextPoint, point, map);
        stateMap[i][j-1] := state;
      end;
    end;
  end;
end;

function TFrmAdvCode.DetermineInsideState(const nextPoint: TPoint;
  const point: TPoint; const map: TStringList): TState;
var
  state: TState;
begin
  case state of
    outside:
      begin
        if (FLoop.Contains(point)) then
        begin
          if(findExits(point, map).contains(nextPoint)) then
            state := followingLoop
          else
            state := crossingIn;
        end;
      end;
    followingLoop:
      begin
        if(findExits(point, map).contains(nextPoint)) then
          state := followingLoop
        else
        begin
          // We're going in or out...
          state := unknown;
        end;
      end;
    unknown:
      begin
        // state does never change...
        // We can go out this time....
        break;
      end;
    crossingIn:
      begin
      if (not FLoop.Contains(point)) then
      begin
        state := inside;
      end
      else
        if(findExits(point, map).contains(nextPoint)) then
          state := followingLoop
        else
          state := crossingOut;
      end;
    inside:
      begin
        if (FLoop.Contains(point)) then
        begin
          if(findExits(point, map).contains(nextPoint)) then
            state := followingLoop
          else
            state := crossingOut;
        end;
      end;
    crossingOut:
      begin
      if (FLoop.Contains(point)) then
      begin
        if(findExits(point, map).contains(nextPoint)) then
          state := followingLoop
        else
          state := crossingIn ;
      end
      else
      begin
        state := outside;
      end;
    end;
  end;
  Result:=state;
end;

end.

