unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Math.Vectors, System.Generics.Collections, System.Math,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Types3D,
  FMX.MaterialSources, FMX.StdCtrls, FMX.Controls.Presentation,
  Gorilla.Mesh, Gorilla.Control, Gorilla.Animation,
  Gorilla.Transform, Gorilla.Model, Gorilla.Controller,
  Gorilla.Controller.Passes.Environment, FMX.Controls3D, Gorilla.Light,
  Gorilla.Viewport, Gorilla.Sphere, Gorilla.Material.Default,
  Gorilla.Material.Custom, Gorilla.Material.Lambert, FMX.Objects;

type
  TForm1 = class(TForm)
    GorillaViewport1: TGorillaViewport;
    GorillaLight1: TGorillaLight;
    GorillaRenderPassEnvironment1: TGorillaRenderPassEnvironment;
    GorillaModel1: TGorillaModel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    CheckBox1: TCheckBox;
    VRRoom: TGorillaModel;
    GorillaLight2: TGorillaLight;
    GorillaLight3: TGorillaLight;
    GorillaLight4: TGorillaLight;
    GorillaLight5: TGorillaLight;
    RoundRect1: TRoundRect;
    CalloutRectangle1: TCalloutRectangle;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    TrackBar1: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  private
    FActiveMesh : TGorillaMesh;
    FAnimators : TList<TPoint3DAnimation>;

    procedure MeshClick(Sender: TObject);
    procedure MeshMouseEnter(Sender: TObject);
    procedure MeshMouseLeave(Sender: TObject);
  public
    procedure DragOver(const Data: TDragObject; const Point: TPointF;
      var Operation: TDragOperation); override;
    procedure DragDrop(const Data: TDragObject;
      const Point: TPointF); override;

    procedure RecreateAnimators(AMesh : TGorillaMesh);
    procedure SetupAnimators(AMesh : TGorillaMesh; AStrength : Single);
    procedure LoadModel(const AFileName : String);
    procedure Play();
    procedure Reverse();
    procedure Reset();
    procedure AdjustCamera();
    procedure ShowMeshInfo(AMesh : TGorillaMesh);
    procedure HideMeshInfo();
  end;

const
  CurrentFilter = '*.dae;*.fbx;*.gltf;*.glb;*.obj;*.stl;*.ply;*.skp' +
    // additional mayo formats
    ';*.step;*.stp;*.brep;*.rle;*.occ;*.iges;*.igs;*.dxf;*.wrl;*.wrz;*.vrml;*.x;*.3mf;*.3ds;*.amf;*.off';

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  System.Masks, System.RTLConsts, FMX.Utils,
  ExplosiveView.Utils,
  Gorilla.Utils.Dialogs, Gorilla.Utils.Math,
  Gorilla.DefTypes,
  Gorilla.DAE.Loader,
  Gorilla.FBX.Loader,
  Gorilla.GLTF.Loader,
  Gorilla.GLB.Loader,
  Gorilla.OBJ.Loader,
  Gorilla.STL.Loader,
  Gorilla.PLY.Loader,
{$IFDEF MSWINDOWS}
  {$IFDEF CPUX64}
  // Sketchup Import available only with Windows x64
  Gorilla.SKP.Loader,
  {$ENDIF}
{$ENDIF}
  Gorilla.Mayo.STEP.Loader,
  Gorilla.Mayo.BREP.Loader,
  Gorilla.Mayo.IGES.Loader,
  Gorilla.Mayo.DXF.Loader,
  Gorilla.Mayo.VRML.Loader,
  Gorilla.Mayo.X.Loader,
  Gorilla.Mayo.X3MF.Loader,
  Gorilla.Mayo.X3DS.Loader,
  Gorilla.Mayo.AMF.Loader,
  Gorilla.Mayo.OFF.Loader;

procedure TForm1.FormCreate(Sender: TObject);
begin
  GorillaViewport1.FrustumCulling := false;

  // Attach light source to designtime camera
  GorillaLight1.Parent := GorillaViewport1.GetDesignCamera();
  GorillaLight1.Position.Point := TPoint3D.Zero;

  // Increase navigation speed
  with GorillaViewport1.GetDesignCameraController() do
  begin
    Interval := 10;
    ShiftIntensity := 5;
    IntensityRotate := 25;
    IntensityZoom := 15;
  end;
end;

procedure TForm1.RecreateAnimators(AMesh : TGorillaMesh);
var I : Integer;
begin
  // Reset mesh positions
  Reset();

  // Recreate animators
  if Assigned(FAnimators) then
  begin
    for I := 0 to FAnimators.Count - 1 do
    begin
      FAnimators[I].Parent := nil;
      FAnimators[I].Free();
    end;

    FAnimators.Clear();
  end;

  SetupAnimators(AMesh, TrackBar1.Value);
end;

procedure TForm1.SetupAnimators(AMesh : TGorillaMesh; AStrength : Single);
const
  EXPLOSIVE_DURATION = 1;

var LAnim : TPoint3DAnimation;
    LParentCenter, LDir,
    LMeshCenterPos : TPoint3D;
    I : Integer;
    LLen : Single;
    LDef : TMeshDef;
    LBBox : TBoundingBox;
    LStrength : TPoint3D;
begin
  // Only set up an animator if there are triangles to be moved
  if Assigned(AMesh.Def) then
  begin
    LDef := AMesh.Def as TMeshDef;
    if Assigned(LDef.IndexSource) and (LDef.IndexSource.Length > 0) then
    begin
      // Assign mouse callbacks
      AMesh.HitTest := true;
      AMesh.OnClick := MeshClick;
      AMesh.OnMouseEnter := MeshMouseEnter;
      AMesh.OnMouseLeave := MeshMouseLeave;

      // Calculate distance to model center to get the direction
      LParentCenter := TPoint3D.Zero;
//        if Assigned(AMesh.Parent) and (AMesh.Parent is TGorillaMesh) then
//          LParentCenter := TGorillaMesh(AMesh.Parent).GetAbsoluteBoundingBox().CenterPoint;

      LBBox := AMesh.GetAbsoluteBoundingBox();
      LMeshCenterPos := LBBox.CenterPoint;
      LLen := LMeshCenterPos.Distance(LParentCenter);
      LDir := (LParentCenter - LMeshCenterPos);
      LDir := LDir.Normalize();

      LAnim := TPoint3DAnimation.Create(AMesh);
      LAnim.Parent := AMesh;
      LAnim.PropertyName := 'Position.Point';
      LAnim.Duration := EXPLOSIVE_DURATION;
      LAnim.StartValue.Point := AMesh.Position.Point;

      LStrength := Point3D(-LLen*AStrength, LLen*AStrength, LLen*AStrength);
      LAnim.StopValue.Point := LAnim.StartValue.Point + (LDir * LStrength);

      FAnimators.Add(LAnim);
    end;
  end;

  if not Assigned(AMesh.Meshes) then
    Exit;

  for I := 0 to AMesh.Meshes.Count - 1 do
    SetupAnimators(AMesh.Meshes.Items[I], AStrength);
end;

procedure TForm1.LoadModel(const AFileName : String);
var LBBox : TBoundingBox;
    LSize : TPoint3D;
    LMaxScale : Single;
begin
  // Reset model, animators and environment render pass
  FreeAndNil(FAnimators);
  GorillaModel1.Clear();
  GorillaModel1.Position.Point := TPoint3D.Zero;
  GorillaModel1.Scale.Point := TPoint3D.Create(1, 1, 1);
  GorillaRenderPassEnvironment1.IgnoredControls.Clear();
  GorillaRenderPassEnvironment1.AllowedControls.Clear();
  CheckBox1.IsChecked := false;

  // Load model
  GorillaModel1.LoadFromFile(nil, AFilename, []);

  // Adjust the size to fit in a cube of size 50
  LBBox := GorillaModel1.GetAbsoluteBoundingBox();
  LSize := LBBox.GetSize();
  LSize.X := 25 / LSize.X;
  LSize.Y := 25 / LSize.Y;
  LSize.Z := 25 / LSize.Z;
  LMaxScale := System.Math.Min(LSize.X, System.Math.Min(LSize.Y, LSize.Z));
  GorillaModel1.Scale.X := LMaxScale;
  GorillaModel1.Scale.Y := LMaxScale;
  GorillaModel1.Scale.Z := LMaxScale;

  GorillaModel1.Position.Y := 0;
  GorillaModel1.RotationAngle.X := 180;

  // Disable mouse interaction generally
  // In SetupAnimators we will allow mouse interaction on specific meshes
  GorillaModel1.SetHitTestValue(false);
  AdjustCamera();

  FAnimators := TList<TPoint3DAnimation>.Create();
  SetupAnimators(GorillaModel1, TrackBar1.Value / LMaxScale);
end;

procedure TForm1.Play();
var I : Integer;
begin
  if not Assigned(FAnimators) then
    Exit;

  Reset();
  for I := 0 to FAnimators.Count - 1 do
  begin
    FAnimators[I].Start();
  end;
end;

procedure TForm1.Reverse();
var I : Integer;
begin
  if not Assigned(FAnimators) then
    Exit;

  for I := 0 to FAnimators.Count - 1 do
  begin
    FAnimators[I].Enabled := false;
    FAnimators[I].Stop();
    FAnimators[I].Inverse := true;
    FAnimators[I].Enabled := true;
    FAnimators[I].Start();
  end;
end;

procedure TForm1.Reset();
var I : Integer;
begin
  if not Assigned(FAnimators) then
    Exit;

  for I := 0 to FAnimators.Count - 1 do
  begin
    FAnimators[I].Stop();
    FAnimators[I].Enabled := false;
    FAnimators[I].Inverse := false;
    TGorillaMesh(FAnimators[I].Parent).Position.Point := FAnimators[I].StartValue.Point;
  end;

  GorillaViewport1.Repaint;
end;

procedure TForm1.DragOver(const Data: TDragObject;
  const Point: TPointF; var Operation: TDragOperation);
begin
  inherited;

  TExplosiveViewUtils.HandleFileDragOver(CurrentFilter, Data, Point,
    Operation);
end;

procedure TForm1.DragDrop(const Data: TDragObject;
  const Point: TPointF);
begin
  inherited;

  TExplosiveViewUtils.HandleFileDragDrop(Data, Point,
    function(const AFileName : String) : Boolean
    begin
      LoadModel(AFileName);
      Result := true;
    end);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Play();
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Reverse();
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  Reset();
end;

procedure TForm1.CheckBox1Change(Sender: TObject);
begin
  Gorilla.Utils.Dialogs.TProgressForm.Open('Rebuilding shaders ...',
    procedure(AForm : TProgressForm)
    begin
      if CheckBox1.IsChecked then
      begin
        GorillaModel1.UnsetAllEnvironmentMappings();

        // Apply environment render pass
        GorillaRenderPassEnvironment1.IsDynamic := true;
        GorillaRenderPassEnvironment1.IgnoreControl(GorillaModel1);
        GorillaModel1.SetEnvironmentMapping(GorillaRenderPassEnvironment1, 1, 0);
      end
      else
      begin
        GorillaModel1.UnsetAllEnvironmentMappings();
      end;
    end);
end;

procedure TForm1.MeshClick(Sender: TObject);
var LMesh : TGorillaMesh;
begin
  LMesh := Sender as TGorillaMesh;

  if Assigned(FActiveMesh) then
    FActiveMesh.IsMarked := false;

  FActiveMesh := LMesh;
  FActiveMesh.EnableDesigning();
  FActiveMesh.IsMarked := true;
end;

procedure TForm1.MeshMouseEnter(Sender: TObject);
var LMesh : TGorillaMesh;
begin
  LMesh := Sender as TGorillaMesh;
  LMesh.Wireframe := true;
  ShowMeshInfo(LMesh);
end;

procedure TForm1.MeshMouseLeave(Sender: TObject);
var LMesh : TGorillaMesh;
begin
  LMesh := Sender as TGorillaMesh;
  LMesh.Wireframe := false;
  HideMeshInfo();
end;

procedure TForm1.AdjustCamera();
var LTotalBox : TBoundingBox;
    LAspect,
    LTanHalfAngleOfView,
    LMaxE  : Single;
    LCamOfs : Single;
begin
  GorillaViewport1.GetDesignCameraController().SetPositionAndAngle(TPoint3D.Zero,
    TPoint3D.Zero);

  // Get the absolute size of our models
  LTotalBox := GorillaModel1.GetAbsoluteBoundingBox();

  // Calculate the correct distance to the object
  LMaxE := System.Math.Max(LTotalBox.Width,
    System.Math.Max(LTotalBox.Height, LTotalBox.Depth));
  LAspect := GorillaViewport1.Width / GorillaViewport1.Height;
  LTanHalfAngleOfView := DegToRad(GorillaViewport1.GetDesignCamera().AngleOfView / 2);
  LTanHalfAngleOfView := Tan(LTanHalfAngleOfView);

  // Place the camera inside of the camera controller, depending on the aspect
  // ratio of our viewport.
  if LAspect > 1 then
  begin
    // Wider than taller
    LCamOfs := LMaxE * (2 * ArcTan(LTanHalfAngleOfView) * LAspect);
    with GorillaViewport1.GetDesignCameraController() do
    begin
      SetPositionAndAngle(LTotalBox.CenterPoint, TPoint3D.Zero);
      Zoom(-LCamOfs);
    end;
  end
  else
  begin
    // Taller than wider
    LCamOfs := LMaxE / (2 * LTanHalfAngleOfView * LAspect);
    with GorillaViewport1.GetDesignCameraController() do
    begin
      SetPositionAndAngle(LTotalBox.CenterPoint, TPoint3D.Zero);
      Zoom(-LCamOfs);
    end;
  end;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  RecreateAnimators(GorillaModel1);
end;

procedure TForm1.ShowMeshInfo(AMesh : TGorillaMesh);
begin
  if not Assigned(AMesh) then
  begin
    HideMeshInfo();
    Exit;
  end;

  CalloutRectangle1.Visible := true;
  Label1.Text := Format('ID: %s', [AMesh.QualifiedName]);
  Label2.Text := Format('Vertices: %d', [TMeshDef(AMesh.Def).VertexSource.Length]);
  Label3.Text := Format('Triangles: %d', [TMeshDef(AMesh.Def).IndexSource.Length div 3]);
  Label4.Text := Format('Volume: %n', [TExplosiveViewUtils.GetVolumeOfMesh(AMesh, TMatrix3D.Identity, 1)]);
  Label5.Text := Format('Surface: %n', [TExplosiveViewUtils.GetSurfaceSizeOfMesh(AMesh, TMatrix3D.Identity, 1)]);
end;

procedure TForm1.HideMeshInfo();
begin
  CalloutRectangle1.Visible := false;
end;

end.
