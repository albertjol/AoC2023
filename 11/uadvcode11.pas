unit uAdvCode11;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, EditBtn, StdCtrls,
  rxspin, Generics.Collections;

type

  { TFrmAdvCode }
  TPoint = record
    X,Y: Integer;
  end;

  TIntList = specialize TList<Integer>;
  TIntList2 = specialize TObjectList<TIntList>;
  TPointList = specialize TList<TPoint>;

  TUniverse = record
    Galaxies: TPointList;
    Map: TIntList2;
  end;

  TFrmAdvCode = class(TForm)
    btn_Start: TButton;
    ed_Answer: TEdit;
    ed_Filename: TFileNameEdit;
    lbl_Filename: TLabel;
    lbl_Answer: TLabel;
    ed_expansionMultiplier: TRxSpinEdit;
    lbl_ExpansionMultiplier: TLabel;

    procedure btn_StartClick(Sender: TObject);
    function parseText(lines: TStringList): TUniverse;
    procedure expandUniverse(universe: TUniverse; amount: Integer);
    function calcDistance(a, b: TPoint; universe: TUniverse): Integer;
    function calcTotalDistances(universe: TUniverse): Int64;

  private
  public

  end;

var
  FrmAdvCode: TFrmAdvCode;

implementation

{$R *.lfm}

{ TFrmAdvCode }

procedure TFrmAdvCode.btn_StartClick(Sender: TObject);
var
  lines: TStringList;
  universe : TUniverse;
  totalDistance: Int64;
begin
  lines := TStringList.Create();
  lines.LoadFromFile(ed_Filename.Text);

  universe := parseText(lines);
  expandUniverse(universe, Trunc(ed_expansionMultiplier.Value));

  totalDistance := calcTotalDistances(universe);

  ed_Answer.Text := IntToStr(totalDistance);
  FreeAndNil(universe.Map);
  FreeAndNil(universe.Galaxies);
  FreeAndNil(lines);
end;

function TFrmAdvCode.parseText(lines: TStringList): TUniverse;
var
  i,j: Integer;
  p: TPoint;
begin
  Result.Map := TIntList2.Create();
  Result.Galaxies := TPointList.Create();
  for i := 0 to lines.Count - 1 do
  begin
    Result.Map.Add(TIntList.Create());
    for j := 1 to lines[0].Length do
    begin
      if lines[i][j] = '.' then
      begin
        Result.Map[i].Add(1);
      end
      else if lines[i][j] = '#' then
      begin
        Result.Map[i].Add(-1);
        // And save the galaxies as we are busy
        p.X := j - 1; // Strings are 1-based...
        p.Y := i;
        Result.Galaxies.Add(p);
      end;
    end;
  end;
end;

function TFrmAdvCode.calcTotalDistances(universe: TUniverse): Int64;
var
  i,j: Integer;
begin
  Result := 0;
  for i := 0 to universe.Galaxies.Count - 1 do
  begin
    for j := i + 1 to universe.Galaxies.Count - 1 do
    begin
      Result := Result + calcDistance(universe.Galaxies[i], universe.Galaxies[j], universe);
    end;
  end;
end;

procedure TFrmAdvCode.expandUniverse(universe: TUniverse; amount: Integer);
var
  i,j : Integer;
  hasGalaxy: Boolean;
begin
  // Vertical expansion
  for i := 0 to universe.Map.Count - 1 do
  begin
    hasGalaxy := False;
    for j := 0 to universe.Map[i].Count - 1 do
    begin
      if universe.Map[i][j] = -1 then
      begin
        hasGalaxy := True;
        break;
      end;
    end;
    if not hasGalaxy then
    begin
      for j := 0 to universe.Map[i].Count - 1 do
      begin
        universe.Map[i][j] := universe.Map[i][j] * amount;
      end;
    end;
  end;

  // Horizontal expansion: identical but different...
  for i := 0 to universe.Map.Count - 1 do
  begin
    hasGalaxy := False;
    for j := 0 to universe.Map[i].Count - 1 do
    begin
      if universe.Map[j][i] = -1 then
      begin
        hasGalaxy := True;
        break;
      end;
    end;
    if not hasGalaxy then
    begin
      for j := 0 to universe.Map[i].Count - 1 do
      begin
         universe.Map[j][i] := universe.Map[j][i] * amount;
         // This can mean that some spots are 'double expanded', but this should
         // not influence the end result because the calculation should never pass
         // 'added' rows/columns
      end;
    end;
  end;
end;

function TFrmAdvCode.calcDistance(a, b: TPoint; universe: TUniverse): Integer;
var
  i: Integer;
  low, high: TPoint;
begin
  if(a.X < b.X) then
  begin
    low := a;
    high := b;
  end
  else
  begin
    low := b;
    high := a;
  end;
  Result := 0;
  for i := low.X + 1 to high.X do
  begin
    Result := Result + Abs(universe.Map[low.Y][i]);
  end;
  for i := a.y + 1 to b.Y do
  begin
    Result := Result + Abs(universe.Map[i][a.X]);
  end;

end;

end.

