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

    { Component events }

    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);

  private
    FAnimators : TList<TPoint3DAnimation>;
    FAnimatorsOriginalExplosion : TList<TPoint3D>;
    FLastPoint : TPointF;
    FInStartingPos : Boolean;

    { Mouse interaction }

    procedure DoOnDragAnimationFinished(ASender : TObject);
    procedure MeshMouseEnter(Sender: TObject);
    procedure MeshMouseLeave(Sender: TObject);
    procedure MeshMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single; RayPos, RayDir: TVector3D);
    procedure MeshMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single; RayPos, RayDir: TVector3D);
    procedure MeshMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single; RayPos, RayDir: TVector3D);

  public
    { Form events }
    procedure DragOver(const Data: TDragObject; const Point: TPointF;
      var Operation: TDragOperation); override;
    procedure DragDrop(const Data: TDragObject;
      const Point: TPointF); override;

    { Animators }
    procedure RecreateAnimators(AMesh : TGorillaMesh);
    procedure SetupAnimators(AMesh : TGorillaMesh; AStrength : Single);
    procedure UpdateAnimatorsCurrentPosition();
    procedure Play();
    procedure Reverse();
    procedure Reset();

    { Model loading and adjustment }

    procedure LoadModel(const AFileName : String);
    procedure ShowMeshInfo(AMesh : TGorillaMesh);
    procedure HideMeshInfo();
    procedure AdjustCamera();
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
  // The Play button will only play from starting position, if we have not
  // dragged parts away or if we're in rewinding mode
  FInStartingPos := true;

  // Because we move mesh parts of our models, the computed bounding boxes
  // are not correct anymore, so it might happen, that meshes disappear due to
  // frustum culling - so we better deactivate it
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

{ Form events }

procedure TForm1.DragOver(const Data: TDragObject;
  const Point: TPointF; var Operation: TDragOperation);
begin
  inherited;

  // Check if the file extension is allowed by Gorilla3D to be loaded
  TExplosiveViewUtils.HandleFileDragOver(CurrentFilter, Data, Point,
    Operation);
end;

procedure TForm1.DragDrop(const Data: TDragObject;
  const Point: TPointF);
begin
  inherited;

  // Start loading the dragged model file (only single files are supported)
  TExplosiveViewUtils.HandleFileDragDrop(Data, Point,
    function(const AFileName : String) : Boolean
    begin
      // This will replace the GorillaModel1 instance and load the model into this
      LoadModel(AFileName);
      Result := true;
    end);
end;

{ Model loading and adjustment }

procedure TForm1.LoadModel(const AFileName : String);
var LBBox : TBoundingBox;
    LSize : TPoint3D;
    LMaxScale : Single;
begin
  // Reset model, animators and environment render pass
  FreeAndNil(FAnimators);
  FreeAndNil(FAnimatorsOriginalExplosion);

  GorillaModel1.Clear();
  GorillaModel1.Position.Point := TPoint3D.Zero;
  GorillaModel1.Scale.Point := TPoint3D.Create(1, 1, 1);
  GorillaRenderPassEnvironment1.IgnoredControls.Clear();
  GorillaRenderPassEnvironment1.AllowedControls.Clear();
  CheckBox1.IsChecked := false;

  // Load model
  var LOpts := TGorillaLoadOptions.Create(AFilename, [], nil, nil);
  LOpts.TextureLimits := TPoint.Create(1024, 1024);
  GorillaModel1.LoadFromFile(LOpts);

  // Adjust the size to fit in a cube of size 25
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

  // Create the lists for our explosion animators
  FAnimators := TList<TPoint3DAnimation>.Create();
  FAnimatorsOriginalExplosion := TList<TPoint3D>.Create();
  SetupAnimators(GorillaModel1, TrackBar1.Value / LMaxScale);
end;

procedure TForm1.ShowMeshInfo(AMesh : TGorillaMesh);
begin
  if not Assigned(AMesh) then
  begin
    HideMeshInfo();
    Exit;
  end;

  // Show the rectangle with current mesh information
  CalloutRectangle1.Visible := true;
  Label1.Text := Format('ID: %s', [AMesh.QualifiedName]);
  Label2.Text := Format('Vertices: %d', [TMeshDef(AMesh.Def).VertexSource.Length]);
  Label3.Text := Format('Triangles: %d', [TMeshDef(AMesh.Def).IndexSource.Length div 3]);
  Label4.Text := Format('Volume: %n', [TExplosiveViewUtils.GetVolumeOfMesh(AMesh, TMatrix3D.Identity, 1)]);
  Label5.Text := Format('Surface: %n', [TExplosiveViewUtils.GetSurfaceSizeOfMesh(AMesh, TMatrix3D.Identity, 1)]);
end;

procedure TForm1.HideMeshInfo();
begin
  // Simply hide the rectangle with the current mesh information
  CalloutRectangle1.Visible := false;
end;

procedure TForm1.AdjustCamera();
var LTotalBox : TBoundingBox;
    LAspect,
    LTanHalfAngleOfView,
    LMaxE  : Single;
    LCamOfs : Single;
begin
  // Reset camera position to zero values
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

{ Animators }

procedure TForm1.RecreateAnimators(AMesh : TGorillaMesh);
var I : Integer;
begin
  // Reset mesh positions
  Reset();

  // Recreate animators by destroying them at first
  // CAUTION: In versions elderly than 1.1.9, there was a bug in TGorillaMesh.Meshes
  // which caused to remove meshes instead of the TPoint3DAnimation only.
  if Assigned(FAnimators) then
  begin
    for I := 0 to FAnimators.Count - 1 do
    begin
      FAnimators[I].Parent := nil;
      FAnimators[I].Free();
    end;

    FAnimators.Clear();
    FAnimatorsOriginalExplosion.Clear();
  end;

  // Recreate animators
  SetupAnimators(AMesh, TrackBar1.Value);
end;

procedure TForm1.SetupAnimators(AMesh : TGorillaMesh; AStrength : Single);
const
  EXPLOSIVE_DURATION = 1;

var LAnim : TPoint3DAnimation;
    LSceneCenter, LDir,
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
      // Assign mouse callbacks to interact on the specific mesh
      AMesh.HitTest := true;
      AMesh.OnMouseEnter := MeshMouseEnter;
      AMesh.OnMouseLeave := MeshMouseLeave;
      AMesh.OnMouseDown := MeshMouseDown;
      AMesh.OnMouseMove := MeshMouseMove;
      AMesh.OnMouseUp := MeshMouseUp;

      // Calculate distance to model center to get the direction ...
      LSceneCenter := TPoint3D.Zero;

      // The explosion direction is the center of the current mesh to the
      // center of the scene
      LBBox := AMesh.GetAbsoluteBoundingBox();
      LMeshCenterPos := LBBox.CenterPoint;
      // To explode depending on the mesh size we need the distance to the center
      LLen := LMeshCenterPos.Distance(LSceneCenter);
      LDir := (LSceneCenter - LMeshCenterPos);
      LDir := LDir.Normalize();

      // Create the explosion animation
      LAnim := TPoint3DAnimation.Create(AMesh);
      LAnim.Parent := AMesh;
      LAnim.PropertyName := 'Position.Point';
      LAnim.Duration := EXPLOSIVE_DURATION;

      // Starting point is the original mesh positions
      LAnim.StartValue.Point := AMesh.Position.Point;

      // Calculate the strength of explosion by the distance and the current
      // trackbar value
      LStrength := Point3D(-LLen * AStrength, LLen * AStrength, LLen * AStrength);
      LAnim.StopValue.Point := LAnim.StartValue.Point + (LDir * LStrength);

      // Save the power of explosion
      AMesh.TagFloat := LLen * AStrength;

      // Add the animator to the global animator list
      FAnimators.Add(LAnim);

      // Save the original stop value so we can restore after user dragging
      FAnimatorsOriginalExplosion.Add(LAnim.StopValue.Point);
    end;
  end;

  if not Assigned(AMesh.Meshes) then
    Exit;

  for I := 0 to AMesh.Meshes.Count - 1 do
    SetupAnimators(AMesh.Meshes.Items[I], AStrength);
end;

procedure TForm1.UpdateAnimatorsCurrentPosition();
var I : Integer;
begin
  // We will set the current mesh position as new stop-value, so animations
  // will start from here, otherwise the result would produce a position jump.
  if not Assigned(FAnimators) then
    Exit;

  for I := 0 to FAnimators.Count - 1 do
    FAnimators[I].StopValue.Point := TGorillaMesh(FAnimators[I].Parent).Position.Point;
end;

procedure TForm1.Play();
var I : Integer;
begin
  if not Assigned(FAnimators) then
    Exit;

  if FInStartingPos then
  begin
    // We should reset all animators with all modified properties.
    // Then we can start the animations.
    Reset();
    for I := 0 to FAnimators.Count - 1 do
      FAnimators[I].Enabled := true;

    FInStartingPos := false;
  end
  else
  begin
    Reverse();
  end;
end;

procedure TForm1.Reverse();
var I : Integer;
begin
  if not Assigned(FAnimators) then
    Exit;

  // Stop animators
  for I := 0 to FAnimators.Count - 1 do
  begin
    FAnimators[I].Enabled := false;
    FAnimators[I].Inverse := true;
  end;

  // Update the current position as new stop value
  UpdateAnimatorsCurrentPosition();

  // Start animators
  for I := 0 to FAnimators.Count - 1 do
    FAnimators[I].Enabled := true;

  FInStartingPos := true;
end;

procedure TForm1.Reset();
var I : Integer;
begin
  if not Assigned(FAnimators) then
    Exit;

  for I := 0 to FAnimators.Count - 1 do
  begin
    FAnimators[I].Enabled := false;
    FAnimators[I].Inverse := false;
    TGorillaMesh(FAnimators[I].Parent).Position.Point := FAnimators[I].StartValue.Point;
    // Reset to original stop explosion position
    FAnimators[I].StopValue.Point := FAnimatorsOriginalExplosion[I];
  end;

  GorillaViewport1.Repaint;

  FInStartingPos := true;
end;

{ Component events }

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
  // Because rebuilding shaders will also recreate textures, this might take a
  // while when having 4K textures. So we better show a dialog during the process.
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

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  RecreateAnimators(GorillaModel1);
end;

{ Mouse interaction }

procedure TForm1.DoOnDragAnimationFinished(ASender : TObject);
var LAnim : TPoint3DAnimation;
    LMesh : TGorillaMesh;
begin
  // The animation was finished, so we can stop and destroy it
  LAnim := (ASender as TPoint3DAnimation);
  LMesh := LAnim.Parent as TGorillaMesh;

  LAnim.Enabled := false;
  // Remove the animation link in the mesh
  LMesh.TagObject := nil;

  // Destroy the animation
  LAnim.Parent := nil;
  LAnim.Free();
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

procedure TForm1.MeshMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single; RayPos, RayDir: TVector3D);
begin
  FLastPoint := TPointF.Create(X, Y);
end;

procedure TForm1.MeshMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single; RayPos, RayDir: TVector3D);
const
  DRAG_DURATION = 1;
  DRAG_INTENSITY = 0.1;
  DRAG_LIMIT = 1;

  function MeshIsDragging(AMesh : TGorillaMesh) : Boolean;
  begin
    Result := Assigned(AMesh.TagObject) and (AMesh.TagObject is TPoint3DAnimation)
      and TPoint3DAnimation(AMesh.TagObject).Enabled
      and TPoint3DAnimation(AMesh.TagObject).Running;
  end;

var LShiftOfs,
    LCurrent : TPointF;
    LDir     : TPoint3D;
    LMesh    : TGorillaMesh;
    LCamera  : TCamera;
    LAnim    : TPoint3DAnimation;
begin
  // Only if the left mouse button is pressed, we allow dragging of any meshes
  if not (ssLeft in Shift) then
  begin
    FLastPoint := TPointF.Create(X, Y);
    Exit;
  end;

  // Get the current mouse position and the selected mesh
  LCurrent := TPointF.Create(X, Y);
  LMesh := Sender as TGorillaMesh;

  // If the mesh is already dragging, we skip here
  if MeshIsDragging(LMesh) then
  begin
    FLastPoint := TPointF.Create(X, Y);
    Exit;
  end;

  // Check if there is an existing dragging animation
  if Assigned(LMesh.TagObject) and (LMesh.TagObject is TPoint3DAnimation) then
  begin
    with TPoint3DAnimation(LMesh.TagObject) do
    begin
      Enabled := false;
      Parent := nil;
      Free();
    end;

    LMesh.TagObject := nil;
  end;

  // Create an animation instance to animate dragging by flying object
  LAnim := TPoint3DAnimation.Create(LMesh);
  LAnim.Parent := LMesh;
  LAnim.PropertyName := 'Position.Point';
  LAnim.Duration := DRAG_DURATION;
  LAnim.Interpolation := TInterpolationType.Linear;
  LAnim.StartValue.Point := LMesh.Position.Point;

  // Get the dragging vector
  LShiftOfs := LCurrent - FLastPoint;
  LShiftOfs.Y := -LShiftOfs.Y;

  // Get the stop position in dragging direction
  LCamera := GorillaViewport1.GetDesignCamera();
  /// At first we apply the left-side direction
  var LLeft := TPoint3D(LCamera.AbsoluteLeft);
  LLeft.Z := -LLeft.Z;
  LDir := LLeft * (LShiftOfs.X * DRAG_INTENSITY);
  /// Then we apply the forward direction
  LDir := LDir + TPoint3D(LCamera.AbsoluteUp) * (LShiftOfs.Y * DRAG_INTENSITY);
  LDir := Clamp(LDir,
    Point3D(-DRAG_LIMIT, -DRAG_LIMIT, -DRAG_LIMIT),
    Point3D(+DRAG_LIMIT, +DRAG_LIMIT, +DRAG_LIMIT)
    );

  // Because the explosion power comes from the mesh distance and the trackbar
  // value, we have stored this value at SetupAnimators, which we can reuse here.
  var LPower := LMesh.TagFloat;
  LAnim.StopValue.Point := LAnim.StartValue.Point + LDir * LPower;

  // Use the OnFinish event to automatically destroy the animation instance
  // when its done
  LAnim.OnFinish := DoOnDragAnimationFinished;
  // Link the animation to the mesh
  LMesh.TagObject := LAnim;
  // Start the animation
  LAnim.Enabled := true;
  // Store the latest mouse position
  FLastPoint := LCurrent;
  // After dragging, we're no longer in starting position
  FInStartingPos := false;
end;

procedure TForm1.MeshMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single; RayPos, RayDir: TVector3D);
begin
  FLastPoint := TPointF.Create(X, Y);
end;

end.
