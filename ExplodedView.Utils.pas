unit ExplodedView.Utils;

interface

uses
  System.Types, System.SysUtils, System.Math, System.Math.Vectors,
  FMX.Types, FMX.Types3D,
  Gorilla.Mesh, Gorilla.Model, Gorilla.Viewport, Gorilla.Controller.Passes.Environment;

type
  TFileDropFunc = reference to function(const AFilename : String) : Boolean;

  TExplodedViewUtils = record
    public
      { Compute the volume of the provided mesh }

      class function GetVolumeOfMesh(const AVBuf : TVertexBuffer;
        const AIBuf : TIndexBuffer; const AScale : TMatrix3D;
        const ASceneScale : Single) : Single; overload; static;
      class function GetVolumeOfMesh(const AMesh : TGorillaMesh;
        const AScale : TMatrix3D; const ASceneScale : Single) : Single; overload; static;

      { Compute the surface size of the provided mesh }

      class function GetSurfaceSizeOfMesh(const AVBuf : TVertexBuffer;
        const AIBuf : TIndexBuffer; const AScale : TMatrix3D;
        const ASceneScale : Single) : Single; overload; static;
      class function GetSurfaceSizeOfMesh(const AMesh : TGorillaMesh;
        const AScale : TMatrix3D; const ASceneScale : Single) : Single; overload; static;

      { File drop of model files }

      class procedure HandleFileDragOver(const AFileFilter : String;
        const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation); static;
      class procedure HandleFileDragDrop(const Data: TDragObject;
        const Point: TPointF; const ACallback : TFileDropFunc); static;

      { Model and camera adjustment }

      class procedure ResetModel(AModel : TGorillaModel); static;
      class procedure AutoAdjustModelSize(AModel : TGorillaModel;
        const AReferenceSize : Single); static;
      class procedure AdjustCamera(AViewport : TGorillaViewport;
        AModel : TGorillaModel); static;

      { Environment Mapping Rendering }

      class procedure ClearEnvironmentRendering(ARenderPass : TGorillaRenderPassEnvironment); static;
      class procedure ToggleEnvironmentRendering(AModel: TGorillaModel;
        ARenderPass : TGorillaRenderPassEnvironment;
        const AEnabled : Boolean); static;
  end;

implementation

uses
  System.Masks, System.RTLConsts, FMX.Utils,
  Gorilla.DefTypes, Gorilla.Utils.Dialogs;

function GetSignedVolumeOfTriangle(const P1, P2, P3 : TPoint3D;
  const ASceneScale : Single) : Single;
var v321, v231, v312, v132, v213, v123 : Single;
begin
  v321 := p3.X * p2.Y * p1.Z;
  v231 := p2.X * p3.Y * p1.Z;
  v312 := p3.X * p1.Y * p2.Z;
  v132 := p1.X * p3.Y * p2.Z;
  v213 := p2.X * p1.Y * p3.Z;
  v123 := p1.X * p2.Y * p3.Z;
  Result := ((1 / 6) * (-v321 + v231 + v312 - v132 - v213 + v123)) / ASceneScale; // DEFAULT_SCENE_SCALE;
end;

class function TExplodedViewUtils.GetVolumeOfMesh(const AVBuf : TVertexBuffer;
  const AIBuf : TIndexBuffer; const AScale : TMatrix3D; const ASceneScale : Single) : Single;
var T : Integer;
    P1, P2, P3 : TPoint3D;
begin
  Result := 0;

  if not Assigned(AVBuf) then
    Exit;
  if not Assigned(AIBuf) then
    Exit;
  if (AVBuf.Length <= 0) then
    Exit;
  if (AIBuf.Length <= 0) then
    Exit;

  T := 0;
  while T < AIBuf.Length do
  begin
    P1 := AVBuf.Vertices[AIBuf.Indices[T]] * AScale;
    P2 := AVBuf.Vertices[AIBuf.Indices[T + 1]] * AScale;
    P3 := AVBuf.Vertices[AIBuf.Indices[T + 2]] * AScale;

    Result := Result + GetSignedVolumeOfTriangle(P1, P2, P3, ASceneScale);
    Inc(T, 3);
  end;

  Result := Abs(Result);
end;

class function TExplodedViewUtils.GetVolumeOfMesh(const AMesh : TGorillaMesh;
  const AScale : TMatrix3D; const ASceneScale : Single) : Single;
var I : Integer;
    LDef : TMeshDef;
    LData : TMeshData;
    LScale : TMatrix3D;
begin
  Result := 0;
  if not Assigned(AMesh) then
    Exit;

  LScale := AScale * TMatrix3D.CreateScaling(AMesh.Scale.Point);

  if AMesh.IsStatic then
  begin
    LDef := (AMesh.Def as TMeshDef);
    Result := TExplodedViewUtils.GetVolumeOfMesh(LDef.VertexSource, LDef.IndexSource,
      LScale, ASceneScale);
  end
  else
  begin
    LData := AMesh.MeshData;
    if Assigned(LData) then
      Result := TExplodedViewUtils.GetVolumeOfMesh(LData.VertexBuffer, LData.IndexBuffer,
        LScale, ASceneScale);
  end;

  if Assigned(AMesh.Meshes) then
  begin
    for I := 0 to AMesh.Meshes.Count - 1 do
      Result := Result + TExplodedViewUtils.GetVolumeOfMesh(AMesh.Meshes[I],
        LScale, ASceneScale);
  end;
end;

class function TExplodedViewUtils.GetSurfaceSizeOfMesh(
  const AMesh: TGorillaMesh; const AScale: TMatrix3D;
  const ASceneScale : Single): Single;
var I : Integer;
    LDef : TMeshDef;
    LData : TMeshData;
    LScale : TMatrix3D;
begin
  Result := 0;
  if not Assigned(AMesh) then
    Exit;

  LScale := AScale * TMatrix3D.CreateScaling(AMesh.Scale.Point);

  if AMesh.IsStatic then
  begin
    LDef := (AMesh.Def as TMeshDef);
    Result := TExplodedViewUtils.GetSurfaceSizeOfMesh(LDef.VertexSource, LDef.IndexSource,
      LScale, ASceneScale);
  end
  else
  begin
    LData := AMesh.MeshData;
    if Assigned(LData) then
      Result := TExplodedViewUtils.GetSurfaceSizeOfMesh(LData.VertexBuffer, LData.IndexBuffer,
        LScale, ASceneScale);
  end;

  if Assigned(AMesh.Meshes) then
  begin
    for I := 0 to AMesh.Meshes.Count - 1 do
      Result := Result + TExplodedViewUtils.GetSurfaceSizeOfMesh(AMesh.Meshes[I],
        LScale, ASceneScale);
  end;
end;

class function TExplodedViewUtils.GetSurfaceSizeOfMesh(
  const AVBuf: TVertexBuffer; const AIBuf: TIndexBuffer;
  const AScale: TMatrix3D; const ASceneScale : Single): Single;

  function SurfaceSizeOfTriangle(const A, B, C: TPoint3D): Single;
  var AB, AC, LCross: TPoint3D;
  begin
    AB := B - A;
    AC := C - A;

    LCross := AB.CrossProduct(AC);
    Result := (0.5 * LCross.Length()) / ASceneScale;
  end;

var T : Integer;
    P1, P2, P3 : TPoint3D;
begin
  Result := 0;

  if not Assigned(AVBuf) then
    Exit;
  if not Assigned(AIBuf) then
    Exit;
  if (AVBuf.Length <= 0) then
    Exit;
  if (AIBuf.Length <= 0) then
    Exit;

  T := 0;
  while T < AIBuf.Length do
  begin
    P1 := AVBuf.Vertices[AIBuf.Indices[T]] * AScale;
    P2 := AVBuf.Vertices[AIBuf.Indices[T + 1]] * AScale;
    P3 := AVBuf.Vertices[AIBuf.Indices[T + 2]] * AScale;

    Result := Result + SurfaceSizeOfTriangle(P1, P2, P3);
    Inc(T, 3);
  end;

  Result := Abs(Result);
end;

class procedure TExplodedViewUtils.HandleFileDragOver(const AFileFilter : String;
  const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation);
var
  Masks, M: string;
  HasFiles: Boolean;
  HasFilter: Boolean;
  HasMatchedFile: Boolean;
  I: Integer;
begin
  inherited;

  // determine if the user is dragging one or more files and
  // if there is any filter set
  HasFiles := Length(Data.Files) > 0;
  Masks := AFileFilter;
  HasFilter := Masks <> '';

  // the Accept value is overriden by the filter only if there is at least one file
  // in the Data drag object; when a filter exists, there must be at least
  // one file that matches the filter in order for Accept to have set by the user;
  // if there is no file matching the filter, Accept is false
  if HasFiles and HasFilter then
  begin
    HasMatchedFile := False;
    M := GetToken(Masks, ';');
    while (M <> '') and (not HasMatchedFile) do
    begin
      for I := 0 to High(Data.Files) do
      begin
        HasMatchedFile := MatchesMask(Data.Files[I], M);

        // there is at least one file matching the filter
        if HasMatchedFile then
          Break;
      end;
      M := GetToken(Masks, ';');
    end;

    if HasMatchedFile then
      Operation := TDragOperation.Move;
  end;
end;

class procedure TExplodedViewUtils.HandleFileDragDrop(const Data: TDragObject;
  const Point: TPointF; const ACallback : TFileDropFunc);
var I : Integer;
    LFileName : String;
begin
  inherited;

  for I := 0 to High(Data.Files) do
  begin
    try
      LFileName := Data.Files[I];

      if Assigned(ACallback) then
      begin
        if not ACallback(LFileName) then
          Continue;
      end;
    except
      on E: Exception do
      begin
        raise Exception.CreateFmt(
          'Failed to load 3D model file: %s'#13#10'ERROR: %s',
          [LFileName, E.Message]);
      end;
    end;
  end;
end;

class procedure TExplodedViewUtils.ResetModel(AModel : TGorillaModel);
begin
  if not Assigned(AModel) then
    Exit;

  // Reset model
  AModel.Clear();
  AModel.Position.Point := TPoint3D.Zero;
  AModel.Scale.Point := TPoint3D.Create(1, 1, 1);
  AModel.ResetRotationAngle();
end;

class procedure TExplodedViewUtils.AutoAdjustModelSize(AModel: TGorillaModel;
  const AReferenceSize: Single);
var LBBox : TBoundingBox;
    LSize : TPoint3D;
    LMaxScale : Single;
begin
  // Adjust the size to fit in a cube of AReferenceSize (f.e. 25.0)
  LBBox := AModel.GetAbsoluteBoundingBox();

  LSize := LBBox.GetSize();
  LSize.X := AReferenceSize / LSize.X;
  LSize.Y := AReferenceSize / LSize.Y;
  LSize.Z := AReferenceSize / LSize.Z;
  LMaxScale := System.Math.Min(LSize.X, System.Math.Min(LSize.Y, LSize.Z));

  AModel.Scale.X := LMaxScale;
  AModel.Scale.Y := LMaxScale;
  AModel.Scale.Z := LMaxScale;

  AModel.Position.Y := 0;
  AModel.RotationAngle.X := 180;
end;

class procedure TExplodedViewUtils.AdjustCamera(AViewport : TGorillaViewport;
  AModel : TGorillaModel);
var LTotalBox : TBoundingBox;
    LAspect,
    LTanHalfAngleOfView,
    LMaxE  : Single;
    LCamOfs : Single;
begin
  // Reset camera position to zero values
  AViewport.GetDesignCameraController().SetPositionAndAngle(TPoint3D.Zero,
    TPoint3D.Zero);

  // Get the absolute size of our models
  LTotalBox := AModel.GetAbsoluteBoundingBox();

  // Calculate the correct distance to the object
  LMaxE := System.Math.Max(LTotalBox.Width,
    System.Math.Max(LTotalBox.Height, LTotalBox.Depth));
  LAspect := AViewport.Width / AViewport.Height;
  LTanHalfAngleOfView := DegToRad(AViewport.GetDesignCamera().AngleOfView / 2);
  LTanHalfAngleOfView := Tan(LTanHalfAngleOfView);

  // Place the camera inside of the camera controller, depending on the aspect
  // ratio of our viewport.
  if LAspect > 1 then
  begin
    // Wider than taller
    LCamOfs := LMaxE * (2 * ArcTan(LTanHalfAngleOfView) * LAspect);
    with AViewport.GetDesignCameraController() do
    begin
      SetPositionAndAngle(LTotalBox.CenterPoint, TPoint3D.Zero);
      Zoom(-LCamOfs);
    end;
  end
  else
  begin
    // Taller than wider
    LCamOfs := LMaxE / (2 * LTanHalfAngleOfView * LAspect);
    with AViewport.GetDesignCameraController() do
    begin
      SetPositionAndAngle(LTotalBox.CenterPoint, TPoint3D.Zero);
      Zoom(-LCamOfs);
    end;
  end;
end;

class procedure TExplodedViewUtils.ToggleEnvironmentRendering(
  AModel: TGorillaModel; ARenderPass : TGorillaRenderPassEnvironment;
  const AEnabled : Boolean);
begin
  if not Assigned(AModel) then
    Exit;
  if not Assigned(ARenderPass) then
    Exit;

  // Because rebuilding shaders will also recreate textures, this might take a
  // while when having 4K textures. So we better show a dialog during the process.
  Gorilla.Utils.Dialogs.TProgressForm.Open('Rebuilding shaders ...',
    procedure(AForm : TProgressForm)
    begin
      if AEnabled then
      begin
        AModel.UnsetAllEnvironmentMappings();

        // Apply environment render pass
        ARenderPass.IsDynamic := true;
        ARenderPass.IgnoreControl(AModel);
        AModel.SetEnvironmentMapping(ARenderPass, 1, 0);
      end
      else
      begin
        AModel.UnsetAllEnvironmentMappings();
      end;
    end);
end;

class procedure TExplodedViewUtils.ClearEnvironmentRendering(
  ARenderPass : TGorillaRenderPassEnvironment);
begin
  ARenderPass.IgnoredControls.Clear();
  ARenderPass.AllowedControls.Clear();
end;

end.
