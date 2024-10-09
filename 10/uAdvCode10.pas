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

  TPointList = specialize TList<TPoint>;

  { TFrmAdvCode }

  TFrmAdvCode = class(TForm)
    btn_Start: TButton;
    ed_Answer: TEdit;
    ed_Filename: TFileNameEdit;
    lbl_Filename: TLabel;
    lbl_Answer: TLabel;

    procedure btn_StartClick(Sender: TObject);
    function findStartPoint(lines: TStringList): TPoint;
    function findExits(point: TPoint; map: TStringList) : T2Points;
    function getChar(point: TPoint; map: TStringList): Char;

  private
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

{ TFrmAdvCode }

procedure TFrmAdvCode.btn_StartClick(Sender: TObject);
var
  lines: TStringList;
  startPoint : TPoint;
  count: Integer = 0;
  exits: T2Points;
  prevPoints: T2Points;
  exitsTmp: T2Points;
begin
  lines := TStringList.Create();
  lines.LoadFromFile(ed_Filename.Text);

  startPoint := findStartPoint(lines);

  prevPoints.a := startPoint;
  prevPoints.b := startPoint;

  exits := findExits(startPoint, lines);

  while (exits.a <> exits.b) do
  begin
    exitsTmp := findExits(exits.a, lines);
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

    exitsTmp := findExits(exits.b, lines);
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
  end;

  ed_Answer.Text := IntToStr(count + 1);

  FreeAndNil(lines);
end;

function TFrmAdvCode.findStartPoint(lines: TStringList): TPoint;
var
  i,j: Integer;
begin
  for i := 0 to lines.Count - 1 do
  begin
    for j := 1 to lines[0].Length do // Strings are 1-based
    begin
      if lines[i][j] = 'S' then
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
  points: TPointList;
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
        points := TPointList.Create();
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

end.

