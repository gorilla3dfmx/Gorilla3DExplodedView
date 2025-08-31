unit ExplodedView.Animator;

interface

uses
  System.Types, System.SysUtils, System.Classes, System.Math, System.Math.Vectors,
  System.Generics.Defaults, System.Generics.Collections,
  FMX.Types, FMX.Types3D, FMX.Controls3D, Gorilla.Mesh, Gorilla.Viewport,
  Gorilla.DefTypes, Gorilla.Animation;

type
  TAnimatorMeshSetupProc = reference to procedure(AMesh : TGorillaMesh);

  TGorillaExplodedViewAnimator = class
  protected
    FViewport : TGorillaViewport;

    FAnimators : TList<TPoint3DAnimation>;
    FAnimatorsOriginalPosition,
    FAnimatorsOriginalExplosion : TList<TPoint3D>;

    FStrength : Single;
    FInStartingPos : Boolean;
    FExpansionChanging : Integer;

    FLastPoint : TPointF;

    procedure DoOnDragAnimationFinished(ASender : TObject);
    procedure DoSetupAnimators(AMesh : TGorillaMesh; const AStrength : Single;
      const AMeshCallback : TAnimatorMeshSetupProc = nil); virtual;
    procedure DoUpdateAnimatorsCurrentPosition(); virtual;

  public
    property Strength : Single read FStrength write FStrength;
    property InStartingPos : Boolean read FInStartingPos write FInStartingPos;
    property ExpansionChanging : Integer read FExpansionChanging;

    constructor Create(AViewport : TGorillaViewport); virtual;
    destructor Destroy(); override;

    procedure Clear();

    { Explosion Controlling }

    procedure RecreateAnimators(AMesh : TGorillaMesh;
      const AStrength : Single);
    procedure SetupAnimators(AMesh : TGorillaMesh; const AStrength : Single;
      const AMeshCallback : TAnimatorMeshSetupProc = nil);

    procedure Play();
    procedure Reverse();
    procedure Reset();

    { Mouse Interaction }

    procedure UpdateLastMousePos(const APoint : TPointF);
    procedure PerformMeshMove(AMesh : TGorillaMesh; const ACurrent : TPointF);
  end;

implementation

uses
  Gorilla.Camera, Gorilla.Utils.Math;

{ TGorillaExplodedViewAnimator }

constructor TGorillaExplodedViewAnimator.Create(AViewport : TGorillaViewport);
begin
  inherited Create();

  FViewport := AViewport;
  FStrength := 1;

  // The Play button will only play from starting position, if we have not
  // dragged parts away or if we're in rewinding mode
  FInStartingPos := true;
end;

destructor TGorillaExplodedViewAnimator.Destroy();
begin
  Clear();

  inherited Destroy();
end;

procedure TGorillaExplodedViewAnimator.Clear();
begin
  FreeAndNil(FAnimators);
  FreeAndNil(FAnimatorsOriginalPosition);
  FreeAndNil(FAnimatorsOriginalExplosion);
end;

procedure TGorillaExplodedViewAnimator.DoOnDragAnimationFinished(ASender : TObject);
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

procedure TGorillaExplodedViewAnimator.RecreateAnimators(AMesh : TGorillaMesh;
  const AStrength : Single);
var I : Integer;
begin
  if FExpansionChanging > 0 then
    Exit;

  FStrength := AStrength;

  AtomicIncrement(FExpansionChanging);
  try
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
      FAnimatorsOriginalPosition.Clear();
      FAnimatorsOriginalExplosion.Clear();
    end;

    // Recreate animators
    SetupAnimators(AMesh, FStrength);
  finally
    AtomicDecrement(FExpansionChanging);
  end;
end;

procedure TGorillaExplodedViewAnimator.DoSetupAnimators(AMesh : TGorillaMesh;
  const AStrength : Single; const AMeshCallback : TAnimatorMeshSetupProc = nil);
const
  EXPLODED_DURATION = 1;

var LAnim : TPoint3DAnimation;
    LSceneCenter, LDir,
    LMeshCenterPos : TPoint3D;
    I : Integer;
    LLen : Single;
    LDef : TMeshDef;
    LBBox : TBoundingBox;
    LStrVal : Single;
    LStrength : TPoint3D;
begin
  FStrength := AStrength;

  // Only set up an animator if there are triangles to be moved
  if Assigned(AMesh.Def) then
  begin
    LDef := AMesh.Def as TMeshDef;
    if Assigned(LDef.IndexSource) and (LDef.IndexSource.Length > 0) then
    begin
      if Assigned(AMeshCallback) then
        AMeshCallback(AMesh);

      // Calculate distance to model center to get the direction ...
      LSceneCenter := TPoint3D.Zero;

      // The explosion direction is the center of the current mesh to the
      // center of the scene
      LBBox := AMesh.GetBoundingBox();
      LMeshCenterPos := LBBox.CenterPoint;
      // To explode depending on the mesh size we need the distance to the center
      LLen := LMeshCenterPos.Distance(LSceneCenter);
      LDir := -(LSceneCenter - LMeshCenterPos);
      LDir := LDir.Normalize();

      // Create the explosion animation
      LAnim := TPoint3DAnimation.Create(AMesh);
      LAnim.Enabled := false;
      LAnim.Parent := AMesh;
      LAnim.PropertyName := 'Position.Point';
      LAnim.Duration := EXPLODED_DURATION;

      // Starting point is the original mesh positions
      LAnim.StartValue.Point := AMesh.Position.Point;

      // Calculate the strength of explosion by the distance and the current
      // trackbar value
      LStrVal := LLen * AStrength;
      LStrength := Point3D(LStrVal, LStrVal, LStrVal);
      LAnim.StopValue.Point := LAnim.StartValue.Point + (LDir * LStrength);

      // Save the power of explosion
      AMesh.TagFloat := LStrVal;

      // Add the animator to the global animator list
      FAnimators.Add(LAnim);

      // Save the original stop value so we can restore after user dragging
      FAnimatorsOriginalPosition.Add(AMesh.Position.Point);
      FAnimatorsOriginalExplosion.Add(LAnim.StopValue.Point);
    end;
  end;

  if not Assigned(AMesh.Meshes) then
    Exit;

  for I := 0 to AMesh.Meshes.Count - 1 do
    DoSetupAnimators(AMesh.Meshes.Items[I], AStrength, AMeshCallback);
end;

procedure TGorillaExplodedViewAnimator.SetupAnimators(AMesh : TGorillaMesh;
  const AStrength : Single; const AMeshCallback : TAnimatorMeshSetupProc = nil);
begin
  Clear();

  FAnimators := TList<TPoint3DAnimation>.Create();
  FAnimatorsOriginalPosition := TList<TPoint3D>.Create();
  FAnimatorsOriginalExplosion := TList<TPoint3D>.Create();
  FInStartingPos := true;

  DoSetupAnimators(AMesh, AStrength, AMeshCallback);
end;

procedure TGorillaExplodedViewAnimator.DoUpdateAnimatorsCurrentPosition();
var I : Integer;
begin
  // We will set the current mesh position as new stop-value, so animations
  // will start from here, otherwise the result would produce a position jump.
  if not Assigned(FAnimators) then
    Exit;

  for I := 0 to FAnimators.Count - 1 do
    FAnimators[I].StopValue.Point := TGorillaMesh(FAnimators[I].Parent).Position.Point;
end;

procedure TGorillaExplodedViewAnimator.Play();
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

procedure TGorillaExplodedViewAnimator.Reverse();
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
  DoUpdateAnimatorsCurrentPosition();

  // Start animators
  for I := 0 to FAnimators.Count - 1 do
    FAnimators[I].Enabled := true;

  FInStartingPos := true;
end;

procedure TGorillaExplodedViewAnimator.Reset();
var I : Integer;
begin
  if not Assigned(FAnimators) then
    Exit;

  for I := 0 to FAnimators.Count - 1 do
  begin
    FAnimators[I].Enabled := false;
    FAnimators[I].Inverse := false;
    TGorillaMesh(FAnimators[I].Parent).Position.Point := FAnimatorsOriginalPosition[I];
    // Reset to original stop explosion position
    FAnimators[I].StopValue.Point := FAnimatorsOriginalExplosion[I];
  end;

  FInStartingPos := true;
end;

procedure TGorillaExplodedViewAnimator.UpdateLastMousePos(const APoint : TPointF);
begin
  FLastPoint := APoint;
end;

procedure TGorillaExplodedViewAnimator.PerformMeshMove(AMesh : TGorillaMesh;
  const ACurrent : TPointF);
const
  DRAG_DURATION = 1;
  DRAG_INTENSITY = 0.1;
  DRAG_LIMIT = 1;

  function MeshIsDragging(AAMesh : TGorillaMesh) : Boolean;
  begin
    Result := Assigned(AAMesh.TagObject) and (AAMesh.TagObject is TPoint3DAnimation)
      and TPoint3DAnimation(AAMesh.TagObject).Enabled
      and TPoint3DAnimation(AAMesh.TagObject).Running;
  end;

var LShiftOfs: TPointF;
    LLeft,
    LDir     : TPoint3D;
    LCamera  : TCamera;
    LAnim    : TPoint3DAnimation;
begin
  if not Assigned(AMesh) then
    Exit;

  // If the mesh is already dragging, we skip here
  if MeshIsDragging(AMesh) then
  begin
    FLastPoint := ACurrent;
    Exit;
  end;

  // Check if there is an existing dragging animation
  if Assigned(AMesh.TagObject) and (AMesh.TagObject is TPoint3DAnimation) then
  begin
    with TPoint3DAnimation(AMesh.TagObject) do
    begin
      Enabled := false;
      Parent := nil;
      Free();
    end;

    AMesh.TagObject := nil;
  end;

  // Create an animation instance to animate dragging by flying object
  LAnim := TPoint3DAnimation.Create(AMesh);
  LAnim.Parent := AMesh;
  LAnim.PropertyName := 'Position.Point';
  LAnim.Duration := DRAG_DURATION;
  LAnim.Interpolation := TInterpolationType.Linear;
  LAnim.StartValue.Point := AMesh.Position.Point;

  // Get the dragging vector
  LShiftOfs := ACurrent - FLastPoint;
  LShiftOfs.Y := -LShiftOfs.Y;

  // Get the stop position in dragging direction
  LCamera := FViewport.GetDesignCamera();

  /// At first we apply the left-side direction
  LLeft := TPoint3D(LCamera.AbsoluteLeft);
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
  var LPower := AMesh.TagFloat;
  LAnim.StopValue.Point := LAnim.StartValue.Point + LDir * LPower;

  // Use the OnFinish event to automatically destroy the animation instance
  // when its done
  LAnim.OnFinish := DoOnDragAnimationFinished;
  // Link the animation to the mesh
  AMesh.TagObject := LAnim;
  // Start the animation
  LAnim.Enabled := true;

  // Store the latest mouse position
  FLastPoint := ACurrent;

  // After dragging, we're no longer in starting position
  FInStartingPos := false;
end;

end.