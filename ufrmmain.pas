unit ufrmMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, OpenGLContext, Forms, Controls, Graphics,
  Dialogs, ComCtrls, ExtCtrls, StdCtrls, gl, uGLParticleEngine;

type

  { TfrmGLTester }

  { TglTexture }

  TglTexture = class
  public
    Width,Height: longint;
    Data        : pointer;
    destructor Destroy; override;
  end;

  TfrmGLTester = class(TForm)
    chkLighting: TCheckBox;
    chkMoveBackground: TCheckBox;
    chkBlending: TCheckBox;
    chkMoveCube: TCheckBox;
    glControl: TOpenGLControl;
    pnlControls: TPanel;
    pnlScene: TPanel;
    stsMain: TStatusBar;
    procedure chkLightingClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure glControlPaint(Sender: TObject);
  private
    lightamb, lightdif, lightpos, light2pos, light2dif,
    light3pos, light3dif, light4pos, light4dif, fogcolor: array [0..3] of GLfloat;
    timer: single;
    LastMsecs: integer;
    rx, ry, rz, rrx, rry, rrz: single;
    textures : array [0..2] of GLuint;    // Storage For 3 Textures
    MyglTextures : array [0..2] of TglTexture;
    CubeList, BackList: GLuint;
    ParticleEngine : TParticleEngine;
    function LoadFileToMemStream(const Filename: string): TMemoryStream;
    function LoadglTexImage2DFromPNG(PNGFilename: string; Image: TglTexture
      ): boolean;
    procedure LoadTextures;
    procedure SetupGL_Lights;
    procedure SetupGL_Shapes;
    procedure SetupGL_ViewPort;
    procedure DrawScene;
    procedure OnIdle(Sender : TObject; var done:boolean);
  public
    { public declarations }
  end;

var
  frmGLTester: TfrmGLTester;

const
  GL_CLAMP_TO_EDGE = $812F;

implementation

uses IntfGraphics, FPimage;

{$R *.lfm}

{ TglTexture }



{ TfrmGLTester }



procedure TfrmGLTester.glControlPaint(Sender: TObject);
begin
  DrawScene;
//
//  glClearColor(0.27, 0.53, 0.71, 1.0); //Set blue background
//
//  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
//  glLoadIdentity;
//  glBegin(GL_TRIANGLES);
//    glColor3f(1, 0, 0);
//    glVertex3f( 0.0, 1.0, 0.0);
//    glColor3f(0, 1, 0);
//    glVertex3f(-1.0,-1.0, 0.0);
//    glColor3f(0, 0, 1);
//    glVertex3f( 1.0,-1.0, 0.0);
//  glEnd;
//  glControl.SwapBuffers;
end;




//----------------- GL Setup stuff ------------------

procedure TfrmGLTester.SetupGL_Lights;
begin
  {init lighting variables}
  {ambient color}
  lightamb[0]:=0.5;
  lightamb[1]:=0.5;
  lightamb[2]:=0.5;
  lightamb[3]:=1.0;
  {diffuse color}
  lightdif[0]:=0.8;
  lightdif[1]:=0.0;
  lightdif[2]:=0.0;
  lightdif[3]:=1.0;
  {diffuse position}
  lightpos[0]:=0.0;
  lightpos[1]:=0.0;
  lightpos[2]:=3.0;
  lightpos[3]:=1.0;
  {diffuse 2 color}
  light2dif[0]:=0.0;
  light2dif[1]:=0.8;
  light2dif[2]:=0.0;
  light2dif[3]:=1.0;
  {diffuse 2 position}
  light2pos[0]:=3.0;
  light2pos[1]:=0.0;
  light2pos[2]:=3.0;
  light2pos[3]:=1.0;
  {diffuse 3 color}
  light3dif[0]:=0.0;
  light3dif[1]:=0.0;
  light3dif[2]:=0.8;
  light3dif[3]:=1.0;
  {diffuse 3 position}
  light3pos[0]:=-3.0;
  light3pos[1]:=0.0;
  light3pos[2]:=0.0;
  light3pos[3]:=1.0;
  {fog color}

  fogcolor[0]:=0.5;
  fogcolor[1]:=0.5;
  fogcolor[2]:=0.5;
  fogcolor[3]:=1.0;

  glLightfv(GL_LIGHT0,GL_AMBIENT,lightamb);
  glLightfv(GL_LIGHT1,GL_AMBIENT,lightamb);
  glLightfv(GL_LIGHT2,GL_DIFFUSE,lightdif);
  glLightfv(GL_LIGHT2,GL_POSITION,lightpos);
  glLightfv(GL_LIGHT3,GL_DIFFUSE,light2dif);
  glLightfv(GL_LIGHT3,GL_POSITION,light2pos);
  glLightfv(GL_LIGHT4,GL_POSITION,light3pos);
  glLightfv(GL_LIGHT4,GL_DIFFUSE,light3dif);
  glEnable(GL_LIGHT0);
  glEnable(GL_LIGHT1);
  glEnable(GL_LIGHT2);
  glEnable(GL_LIGHT3);
  glEnable(GL_LIGHT4);

end;

procedure TfrmGLTester.SetupGL_Shapes;
var
  i: Integer;
begin
  glGenTextures(3, @textures[0]);
  for i:=0 to 2 do begin
    glBindTexture(GL_TEXTURE_2D, Textures[i]);
    glTexParameterf(GL_TEXTURE_2D,GL_TEXTURE_WRAP_S,GL_CLAMP_TO_EDGE);
    glTexParameterf(GL_TEXTURE_2D,GL_TEXTURE_WRAP_T,GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR);
    glTexImage2D(GL_TEXTURE_2D,0,3,MyglTextures[i].Width,MyglTextures[i].Height,0
        ,GL_RGB,GL_UNSIGNED_BYTE,MyglTextures[i].Data);
  end;
  glTexEnvf(GL_TEXTURE_ENV,GL_TEXTURE_ENV_MODE,GL_MODULATE);
  {instead of GL_MODULATE you can try GL_DECAL or GL_BLEND}
  glEnable(GL_TEXTURE_2D);          // enables 2d textures
  glClearColor(0.0,0.0,0.0,1.0);    // sets background color
  glClearDepth(1.0);
  glDepthFunc(GL_LEQUAL);           // the type of depth test to do
  glEnable(GL_DEPTH_TEST);          // enables depth testing
  glShadeModel(GL_SMOOTH);          // enables smooth color shading
  {blending}
  glColor4f(1.0,1.0,1.0,0.5);       // Full Brightness, 50% Alpha ( NEW )
  glBlendFunc(GL_SRC_ALPHA, GL_ONE);
  glLightModeli(GL_LIGHT_MODEL_LOCAL_VIEWER, GL_TRUE);
  {}
  glHint(GL_LINE_SMOOTH_HINT,GL_NICEST);
  glHint(GL_POLYGON_SMOOTH_HINT,GL_NICEST);
  glHint(GL_PERSPECTIVE_CORRECTION_HINT,GL_NICEST);

  // creating display lists

  ParticleEngine.ParticleList :=glGenLists(1);
  glNewList(ParticleEngine.ParticleList, GL_COMPILE);
    glBindTexture(GL_TEXTURE_2D, textures[0]);
    glBegin(GL_TRIANGLE_STRIP);
      glNormal3f( 0.0, 0.0, 1.0);
      glTexCoord2f( 1.0, 1.0);     glVertex3f(+0.025, +0.025, 0);
      glTexCoord2f( 0.0, 1.0);     glVertex3f(-0.025, +0.025, 0);
      glTexCoord2f( 1.0, 0.0);     glVertex3f(+0.025, -0.025, 0);
      glTexCoord2f( 0.0, 0.0);     glVertex3f(-0.025, -0.025, 0);
    glEnd;
  glEndList;

  BackList:=ParticleEngine.ParticleList+1;
  glNewList(BackList, GL_COMPILE);
    glBindTexture(GL_TEXTURE_2D, textures[2]);
    glBegin(GL_QUADS);
      {Front Face}
      glNormal3f( 0.0, 0.0, 1.0);
      glTexCoord2f( 1.0, 1.0);     glVertex3f( 2.5, 2.5, 2.5);
      glTexCoord2f( 0.0, 1.0);     glVertex3f(-2.5, 2.5, 2.5);
      glTexCoord2f( 0.0, 0.0);     glVertex3f(-2.5,-2.5, 2.5);
      glTexCoord2f( 1.0, 0.0);     glVertex3f( 2.5,-2.5, 2.5);
      {Back Face}
      glNormal3f( 0.0, 0.0,-1.0);
      glTexCoord2f( 0.0, 1.0);     glVertex3f( 2.5, 2.5,-2.5);
      glTexCoord2f( 0.0, 0.0);     glVertex3f( 2.5,-2.5,-2.5);
      glTexCoord2f( 1.0, 0.0);     glVertex3f(-2.5,-2.5,-2.5);
      glTexCoord2f( 1.0, 1.0);     glVertex3f(-2.5, 2.5,-2.5);
      {Left Face}
      glNormal3f(-1.0, 0.0, 0.0);
      glTexCoord2f( 1.0, 1.0);     glVertex3f(-2.5, 2.5, 2.5);
      glTexCoord2f( 0.0, 1.0);     glVertex3f(-2.5, 2.5,-2.5);
      glTexCoord2f( 0.0, 0.0);     glVertex3f(-2.5,-2.5,-2.5);
      glTexCoord2f( 1.0, 0.0);     glVertex3f(-2.5,-2.5, 2.5);
      {Right Face}
      glNormal3f( 1.0, 0.0, 0.0);
      glTexCoord2f( 1.0, 1.0);     glVertex3f( 2.5, 2.5,-2.5);
      glTexCoord2f( 0.0, 1.0);     glVertex3f( 2.5, 2.5, 2.5);
      glTexCoord2f( 0.0, 0.0);     glVertex3f( 2.5,-2.5, 2.5);
      glTexCoord2f( 1.0, 0.0);     glVertex3f( 2.5,-2.5,-2.5);
      {Top Face}
      glNormal3f( 0.0, 1.0, 0.0);
      glTexCoord2f( 1.0, 1.0);     glVertex3f( 2.5, 2.5,-2.5);
      glTexCoord2f( 0.0, 1.0);     glVertex3f(-2.5, 2.5,-2.5);
      glTexCoord2f( 0.0, 0.0);     glVertex3f(-2.5, 2.5, 2.5);
      glTexCoord2f( 1.0, 0.0);     glVertex3f( 2.5, 2.5, 2.5);
      {Bottom Face}
      glNormal3f( 0.0,-1.0, 0.0);
      glTexCoord2f( 1.0, 1.0);     glVertex3f(-2.5,-2.5,-2.5);
      glTexCoord2f( 0.0, 1.0);     glVertex3f( 2.5,-2.5,-2.5);
      glTexCoord2f( 0.0, 0.0);     glVertex3f( 2.5,-2.5, 2.5);
      glTexCoord2f( 1.0, 0.0);     glVertex3f(-2.5,-2.5, 2.5);

    glEnd;
  glEndList;

  CubeList:=BackList+1;
  glNewList(CubeList, GL_COMPILE);
    glBindTexture(GL_TEXTURE_2D, textures[1]);
    glBegin(GL_QUADS);
      {Front Face}
      glNormal3f( 0.0, 0.0, 1.0);
      glTexCoord2f( 1.0, 1.0);     glVertex3f( 0.5, 0.5, 0.5);
      glTexCoord2f( 0.0, 1.0);     glVertex3f(-0.5, 0.5, 0.5);
      glTexCoord2f( 0.0, 0.0);     glVertex3f(-0.5,-0.5, 0.5);
      glTexCoord2f( 1.0, 0.0);     glVertex3f( 0.5,-0.5, 0.5);
      {Back Face}
      glNormal3f( 0.0, 0.0,-1.0);
      glTexCoord2f( 0.0, 1.0);     glVertex3f( 0.5, 0.5,-0.5);
      glTexCoord2f( 0.0, 0.0);     glVertex3f( 0.5,-0.5,-0.5);
      glTexCoord2f( 1.0, 0.0);     glVertex3f(-0.5,-0.5,-0.5);
      glTexCoord2f( 1.0, 1.0);     glVertex3f(-0.5, 0.5,-0.5);
    glEnd;
    glBindTexture(GL_TEXTURE_2D, textures[1]);
    glBegin(GL_QUADS);
      {Left Face}
      glNormal3f(-1.0, 0.0, 0.0);
      glTexCoord2f( 1.0, 1.0);     glVertex3f(-0.5, 0.5, 0.5);
      glTexCoord2f( 0.0, 1.0);     glVertex3f(-0.5, 0.5,-0.5);
      glTexCoord2f( 0.0, 0.0);     glVertex3f(-0.5,-0.5,-0.5);
      glTexCoord2f( 1.0, 0.0);     glVertex3f(-0.5,-0.5, 0.5);
      {Right Face}
      glNormal3f( 1.0, 0.0, 0.0);
      glTexCoord2f( 1.0, 1.0);     glVertex3f( 0.5, 0.5,-0.5);
      glTexCoord2f( 0.0, 1.0);     glVertex3f( 0.5, 0.5, 0.5);
      glTexCoord2f( 0.0, 0.0);     glVertex3f( 0.5,-0.5, 0.5);
      glTexCoord2f( 1.0, 0.0);     glVertex3f( 0.5,-0.5,-0.5);
    glEnd;
    glBindTexture(GL_TEXTURE_2D, textures[2]);
    glBegin(GL_QUADS);
      {Top Face}
      glNormal3f( 0.0, 1.0, 0.0);
      glTexCoord2f( 1.0, 1.0);     glVertex3f( 0.5, 0.5,-0.5);
      glTexCoord2f( 0.0, 1.0);     glVertex3f(-0.5, 0.5,-0.5);
      glTexCoord2f( 0.0, 0.0);     glVertex3f(-0.5, 0.5, 0.5);
      glTexCoord2f( 1.0, 0.0);     glVertex3f( 0.5, 0.5, 0.5);
      {Bottom Face}
      glNormal3f( 0.0,-1.0, 0.0);
      glTexCoord2f( 1.0, 1.0);     glVertex3f(-0.5,-0.5,-0.5);
      glTexCoord2f( 0.0, 1.0);     glVertex3f( 0.5,-0.5,-0.5);
      glTexCoord2f( 0.0, 0.0);     glVertex3f( 0.5,-0.5, 0.5);
      glTexCoord2f( 1.0, 0.0);     glVertex3f(-0.5,-0.5, 0.5);
    glEnd;
  glEndList;

end;

procedure TfrmGLTester.SetupGL_ViewPort;
begin
  glMatrixMode (GL_PROJECTION);    { prepare for and then }
  glLoadIdentity ();               { define the projection }
  glFrustum (-1.0, 1.0, -1.0, 1.0, 1.5, 20.0); { transformation }
  glMatrixMode (GL_MODELVIEW);  { back to modelview matrix }
  //glViewport (0, 0, glControl.Width, glControl.Height);
                                { define the viewport }
end;

procedure TfrmGLTester.DrawScene;
var
  CurTime: TDateTime;
  MSecs: integer;
begin
  //inc(FrameCount);
  //inc(LastFrameTicks,OpenGLControl1.FrameDiffTimeInMSecs);
  //if (LastFrameTicks>=1000) then begin
  //  DebugLn(['TExampleForm.OpenGLControl1Paint Frames per second: ',FrameCount]);
  //  dec(LastFrameTicks,1000);
  //  FrameCount:=0;
  //end;
  //


    CurTime:=Now;
    MSecs:=round(CurTime*86400*1000) mod 1000;
    if MSecs<0 then MSecs:=1000+MSecs;
    timer:=msecs-LastMsecs;
    if timer<0 then timer:=1000+timer;
    LastMsecs:=MSecs;

    ParticleEngine.MoveParticles(timer);

    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
    glLoadIdentity;             { clear the matrix }
    glTranslatef (0.0, 0.0,-3.0);  // -2.5); { viewing transformation }
    {rotate}

    glPushMatrix;

    if chkMoveBackground.Checked then
    begin
      rrx:=rrx-0.6*(timer/10);
      rry:=rry-0.5*(timer/10);
      rrz:=rrz-0.3*(timer/10);
    end;

    glRotatef(rrx,1.0,0.0,0.0);
    glRotatef(rry,0.0,1.0,0.0);
    glRotatef(rrz,0.0,0.0,1.0);

    // draw background
    if chkBlending.Checked then
    begin
      glEnable(GL_BLEND);
      glDisable(GL_DEPTH_TEST);
    end;
    glCallList(BackList);

    glPopMatrix;

    glPushMatrix;

    if chkMoveCube.Checked then
    begin
      rx:=rx+0.5*(timer/10);
      ry:=ry+0.25*(timer/10);
      rz:=rz+0.8*(timer/10);
    end;

    glRotatef(rx,1.0,0.0,0.0);
    glRotatef(ry,0.0,1.0,0.0);
    glRotatef(rz,0.0,0.0,1.0);

    // draw cube
    glCallList(CubeList);
    if chkBlending.Checked then
    begin
      glDisable(GL_BLEND);
      glEnable(GL_DEPTH_TEST);
    end;

    glPopMatrix;

    if chkBlending.Checked then glEnable(GL_BLEND);
    ParticleEngine.DrawParticles;
    if chkBlending.Checked then glDisable(GL_BLEND);
    //glFlush;
    //glFinish;
    // Swap backbuffer to front
    glControl.SwapBuffers;

end;

procedure TfrmGLTester.LoadTextures;

  procedure LoadglTexture(Filename:string; Image:TglTexture);
  begin
    Filename:=ExpandFileNameUTF8(Filename);
    if not LoadglTexImage2DFromPNG(Filename,Image) then begin
      MessageDlg('File not found',
        'Image file not found: '+Filename,
        mtError,[mbOk],0);
      raise Exception.Create('Image file not found: '+Filename);
    end;
  end;

var
  i: Integer;
begin
  for i:=0 to 2 do begin
    Textures[i]:=0;
    MyglTextures[i]:=TglTexture.Create;
  end;
  {loading the texture and setting its parameters}

  LoadglTexture('data/particle.png',MyglTextures[0]);
  LoadglTexture('data/texture2.png',MyglTextures[1]);
  LoadglTexture('data/texture3.png',MyglTextures[2]);
end;

//----------------- app plumbing ---------------------

procedure TfrmGLTester.FormShow(Sender: TObject);
begin
  ParticleEngine:=TParticleEngine.Create;

  glControl.MakeCurrent;
  LoadTextures;
  SetupGL_Lights;
  SetupGL_Shapes;
  SetupGL_Viewport;
  ParticleEngine.Start;

  Application.OnIdle := @OnIdle;
  stsMain.SimpleText:= format('Open GL %d.%d',[glControl.OpenGLMajorVersion,glControl.OpenGLMinorVersion]);
end;

procedure TfrmGLTester.chkLightingClick(Sender: TObject);
begin
  if chkLighting.checked then
    glEnable(GL_LIGHTING)
  else
    glDisable(GL_LIGHTING);
end;

procedure TfrmGLTester.FormDestroy(Sender: TObject);
var i: integer;
begin
  for i:=0 to 2 do begin
    Textures[i]:=0;
    FreeAndNil(MyglTextures[i]);
  end;
  FreeAndNil(ParticleEngine);
end;

procedure TfrmGLTester.OnIdle(Sender: TObject; var done: boolean);
begin
  glControl.Invalidate;
  //OpenGLControl1Paint(Self);
  done:=false; // tell lcl to handle messages and return immediatly
end;



destructor TglTexture.Destroy;
begin
  if Data<>nil then FreeMem(Data);
  inherited Destroy;
end;

function TfrmGLTester.LoadFileToMemStream(const Filename: string): TMemoryStream;
var FileStream: TFileStream;
begin
  Result:=TMemoryStream.Create;
  try
    FileStream:=TFileStream.Create(UTF8ToSys(Filename), fmOpenRead);
    try
      Result.CopyFrom(FileStream,FileStream.Size);
      Result.Position:=0;
    finally
      FileStream.Free;
    end;
  except
    Result.Free;
    Result:=nil;
  end;
end;

function TfrmGLTester.LoadglTexImage2DFromPNG(PNGFilename: string; Image: TglTexture
  ): boolean;
var
  png: TPortableNetworkGraphic;
  IntfImg: TLazIntfImage;
  y: Integer;
  x: Integer;
  c: TFPColor;
  p: PByte;
begin
  Result:=false;
  png:=TPortableNetworkGraphic.Create;
  IntfImg:=nil;
  try
    png.LoadFromFile(PNGFilename);
    IntfImg:=png.CreateIntfImage;
    Image.Width:=IntfImg.Width;
    Image.Height:=IntfImg.Height;
    GetMem(Image.Data,Image.Width*Image.Height * 3);
    p:=PByte(Image.Data);
    for y:=0 to IntfImg.Height-1 do begin
      for x:=0 to IntfImg.Width-1 do begin
        c:=IntfImg.Colors[x,y];
        p^:=c.red shr 8;
        inc(p);
        p^:=c.green shr 8;
        inc(p);
        p^:=c.blue shr 8;
        inc(p);
      end;
    end;
  finally
    png.Free;
    IntfImg.Free;
  end;
  Result:=true;
end;

end.

