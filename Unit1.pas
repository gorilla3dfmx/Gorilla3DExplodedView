unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Math.Vectors, System.Generics.Collections, System.Math,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Types3D,
  FMX.MaterialSources, FMX.StdCtrls, FMX.Controls.Presentation, FMX.Objects,
  Gorilla.Mesh, Gorilla.Control, Gorilla.Animation,
  Gorilla.Transform, Gorilla.Model, Gorilla.Controller,
  Gorilla.Controller.Passes.Environment, FMX.Controls3D, Gorilla.Light,
  Gorilla.Viewport, Gorilla.Sphere, Gorilla.Material.Default,
  Gorilla.Material.Custom, Gorilla.Material.Lambert,
  ExplodedView.Animator;

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
    procedure CheckBox1Change(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

  private
    FAnimator : TGorillaExplodedViewAnimator;

    { Mouse interaction }

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

    { Model loading and adjustment }

    procedure LoadModel(const AFileName : String);
    procedure ShowMeshInfo(AMesh : TGorillaMesh);
    procedure HideMeshInfo();
  end;

const
  CurrentFilter = '*.g3d;*.dae;*.fbx;*.gltf;*.glb;*.obj;*.stl;*.ply;*.skp' +
    // additional mayo formats
    ';*.step;*.stp;*.brep;*.rle;*.occ;*.iges;*.igs;*.dxf;*.wrl;*.wrz;*.vrml;*.x;*.3mf;*.3ds;*.amf;*.off';

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  System.Masks, System.RTLConsts, FMX.Utils,
  ExplodedView.Utils, Gorilla.Camera,
  Gorilla.Utils.Dialogs, Gorilla.Utils.Math,
  Gorilla.DefTypes,
  Gorilla.G3D.Loader,
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
  // Because we move mesh parts of our models, the computed bounding boxes
  // are not correct anymore, so it might happen, that meshes disappear due to
  // frustum culling - so we better deactivate it
  GorillaViewport1.FrustumCulling := false;

  // Attach light source to designtime camera
  GorillaLight1.Parent := GorillaViewport1.GetDesignCamera();
  GorillaLight1.Position.Point := TPoint3D.Zero;

  // Configure camera navigation with our default smooth controller
  with GorillaViewport1.GetDesignCameraController() do
  begin
    // Activate orbiting camera navigation (available since v1.1.9)
    NavigationProfile := TGorillaCameraNavigationProfile.cnpOrbit;

    // Increase navigation speed
    Interval := 10;
    ShiftIntensity := 5;
    IntensityRotate := 25;
    IntensityZoom := 15;
  end;

  // Hide the mesh information rectangle
  HideMeshInfo();

  // Create the animator, which is responsible for the exploded view controlling
  FAnimator := TGorillaExplodedViewAnimator.Create(GorillaViewport1);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FAnimator);
end;

{ Form events }

procedure TForm1.DragOver(const Data: TDragObject;
  const Point: TPointF; var Operation: TDragOperation);
begin
  inherited;

  // Check if the file extension is allowed by Gorilla3D to be loaded
  TExplodedViewUtils.HandleFileDragOver(CurrentFilter, Data, Point,
    Operation);
end;

procedure TForm1.DragDrop(const Data: TDragObject;
  const Point: TPointF);
begin
  inherited;

  // Start loading the dragged model file (only single files are supported)
  TExplodedViewUtils.HandleFileDragDrop(Data, Point,
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
  // Reset model
  TExplodedViewUtils.ResetModel(GorillaModel1);

  // Reset environment rendering
  TExplodedViewUtils.ClearEnvironmentRendering(GorillaRenderPassEnvironment1);
  CheckBox1.IsChecked := false;

  // Load model
  var LOpts := TGorillaLoadOptions.Create(AFilename, [], nil, nil);
  LOpts.TextureLimits := TPoint.Create(1024, 1024);
  GorillaModel1.LoadFromFile(LOpts);

  // Adjust the size to fit in a cube of size 25
  TExplodedViewUtils.AutoAdjustModelSize(GorillaModel1, 25);

  // Disable mouse interaction generally
  // In SetupAnimators we will allow mouse interaction on specific meshes
  GorillaModel1.SetHitTestValue(false);

  // Adjust the camera and model size to have a good starting position
  TExplodedViewUtils.AdjustCamera(GorillaViewport1, GorillaModel1);

  // Create the lists for our explosion animators
  FAnimator.SetupAnimators(GorillaModel1, TrackBar1.Value,
    procedure(AMesh : TGorillaMesh)
    begin
      // Assign mouse callbacks to interact on the specific mesh
      AMesh.HitTest := true;
      AMesh.OnMouseEnter := MeshMouseEnter;
      AMesh.OnMouseLeave := MeshMouseLeave;
      AMesh.OnMouseDown := MeshMouseDown;
      AMesh.OnMouseMove := MeshMouseMove;
      AMesh.OnMouseUp := MeshMouseUp;

      AMesh.Hint := AMesh.QualifiedName;
      AMesh.ShowHint := true;
    end);
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
  Label1.Text := Format('ID: %s', [Copy(AMesh.QualifiedName, 1, 32)]);
  Label2.Text := Format('Vertices: %d', [TMeshDef(AMesh.Def).VertexSource.Length]);
  Label3.Text := Format('Triangles: %d', [TMeshDef(AMesh.Def).IndexSource.Length div 3]);
  Label4.Text := Format('Volume: %n', [TExplodedViewUtils.GetVolumeOfMesh(AMesh, TMatrix3D.Identity, 1)]);
  Label5.Text := Format('Surface: %n', [TExplodedViewUtils.GetSurfaceSizeOfMesh(AMesh, TMatrix3D.Identity, 1)]);
end;

procedure TForm1.HideMeshInfo();
begin
  // Simply hide the rectangle with the current mesh information
  CalloutRectangle1.Visible := false;
end;

{ Component events }

procedure TForm1.Button1Click(Sender: TObject);
begin
  FAnimator.Play();
end;

procedure TForm1.CheckBox1Change(Sender: TObject);
begin
  // Toggle (enable / disable) environment rendering
  TExplodedViewUtils.ToggleEnvironmentRendering(GorillaModel1,
    GorillaRenderPassEnvironment1, CheckBox1.IsChecked);
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  // Change the strength of the explosion
  FAnimator.RecreateAnimators(GorillaModel1, TrackBar1.Value);
end;

{ Mouse interaction }

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
  FAnimator.UpdateLastMousePos(PointF(X, Y));
end;

procedure TForm1.MeshMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single; RayPos, RayDir: TVector3D);
var LMesh : TGorillaMesh;
begin
  // Only if the left mouse button is pressed, we allow dragging of any meshes
  if not (ssLeft in Shift) then
  begin
    FAnimator.UpdateLastMousePos(PointF(X, Y));
    Exit;
  end;

  LMesh := Sender as TGorillaMesh;
  FAnimator.PerformMeshMove(LMesh, PointF(X, Y));
end;

procedure TForm1.MeshMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single; RayPos, RayDir: TVector3D);
begin
  FAnimator.UpdateLastMousePos(TPointF.Create(X, Y));
end;

end.
