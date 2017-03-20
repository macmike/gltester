unit uGLParticleEngine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gl;

type

  TParticle = class
    x, y, z: GLfloat;
    vx, vy, vz: GLfloat;
    life: single;
  end;

  { TParticleEngine }

  TParticleEngine = class
    xspawn: GLfloat;
    Particle: array [1..2001] of TParticle;
    procedure MoveParticles(const atimer : single);
    procedure DrawParticles;
    procedure Start;
  private
    FListNumber: GLuint;
    Fdirection : boolean;
    FTimer: single;
  public
    constructor Create;
    destructor Destroy; override;
    property ParticleList : GLuint read FListNumber write FListNumber;
    property Timer : single read FTimer write FTimer;
  private
    procedure RespawnParticle(i: integer);
  end;

implementation

constructor TParticleEngine.Create;
var i: integer;
begin
  for i:=1 to 2001 do Particle[i]:=TParticle.Create;
  xspawn:=0;
end;

destructor TParticleEngine.Destroy;
var i: integer;
begin
  for i:=1 to 2001 do FreeAndNil(Particle[i]);
  inherited Destroy;
end;

procedure TParticleEngine.DrawParticles;
var i: integer;
begin
  for i:=1 to 2001 do begin
    glPushMatrix;
    glTranslatef(Particle[i].x, Particle[i].y, Particle[i].z);
    glCallList(ParticleList);
    glPopMatrix;
  end;
end;

procedure TParticleEngine.RespawnParticle(i: integer);
begin
  if (xspawn>2) and (Fdirection=true) then Fdirection:=false;
  if (xspawn<-2) and (Fdirection=false) then Fdirection:=true;
  if Fdirection then
    xspawn:=xspawn+0.0002*(timer/10)
  else
    xspawn:=xspawn-0.0002*(timer/10);
  Particle[i].x:=xspawn;
  Particle[i].y:=-0.5;
  Particle[i].z:=0;
  Particle[i].vx:=-0.005+GLFloat(random(2000))/200000;
  Particle[i].vy:=0.035+GLFloat(random(750))/100000;
  Particle[i].vz:=-0.005+GLFloat(random(2000))/200000;
  Particle[i].life:=GLFloat(random(1250))/1000+1;
end;

procedure TParticleEngine.MoveParticles(const atimer: single);
var i: integer;
begin
  FTimer:=atimer;
  for i:=1 to 2001 do begin
    if Particle[i].life>0 then begin
      Particle[i].life:=Particle[i].life-0.01*(timer/10);
      Particle[i].x:=Particle[i].x+Particle[i].vx*(timer/10);

      Particle[i].vy:=Particle[i].vy-0.00035*(timer/10); // gravity
      Particle[i].y:=Particle[i].y+Particle[i].vy*(timer/10);

      Particle[i].z:=Particle[i].z+Particle[i].vz*(timer/10);
    end else begin
      RespawnParticle(i);
    end;
  end;
end;

procedure TParticleEngine.Start;
var i: integer;
begin
  for i:=1 to 2001 do begin
    RespawnParticle(i);
  end;
end;


end.

