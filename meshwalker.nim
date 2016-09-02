import openmesh

type
  HalfedgeWalker = object
    mesh: ptr BaseMesh
    handle: HalfedgeHandle

  VertexWalker = object
    mesh: ptr BaseMesh
    handle: VertexHandle

  FaceWalker = object
    mesh: ptr BaseMesh
    handle: FaceHandle

  EdgeWalker = object
    mesh: ptr BaseMesh
    handle: EdgeHandle

#proc isValid(walker: HalfedgeWalker | VertexWalker | FaceWalker | EdgeWalker): bool =
#  walker.handle.isValid

proc goNext*(walker: HalfedgeWalker): HalfedgeWalker =
  result.mesh = walker.mesh
  result.handle = walker.mesh[walker.handle].next_halfedge_handle

proc goPrev*(walker: HalfedgeWalker): HalfedgeWalker =
  result.mesh = walker.mesh
  result.handle = walker.mesh[walker.handle].prev_halfedge_handle

proc goOpp*(walker: HalfedgeWalker): HalfedgeWalker =
  result.mesh = walker.mesh
  result.handle = HalfedgeHandle(walker.handle.int xor 1)

proc goToVertex*(walker: HalfedgeWalker): VertexWalker =
  result.mesh = walker.mesh
  result.handle = walker.mesh[walker.handle].vertex_handle

proc goFromVertex*(walker: HalfedgeWalker): VertexWalker =
  walker.goOpp.goToVertex

proc goFace*(walker: HalfedgeWalker): FaceWalker =
  result.mesh = walker.mesh
  result.handle = walker.mesh[walker.handle].face_handle

proc goEdge*(walker: HalfedgeWalker): EdgeWalker =
  result.mesh = walker.mesh
  result.handle = EdgeHandle(walker.handle.int div 2)

proc goOutHalfedge*(walker: VertexWalker): HalfedgeWalker =
  result.mesh   = walker.mesh
  result.handle = walker.mesh[walker.handle].out_halfedge_handle

proc goHalfedge*(walker: FaceWalker): HalfedgeWalker =
  result.mesh   = walker.mesh
  result.handle = walker.mesh[walker.handle].halfedge_handle

proc goHalfedge*(walker: EdgeWalker): HalfedgeWalker =
  result.mesh   = walker.mesh
  result.handle = HalfedgeHandle(walker.handle.int * 2)

#### vertex circulators ####

iterator circulateVertices*(walker: VertexWalker): VertexWalker =
  var startHeWalker = walker.goOutHalfedge
  
  if startHeWalker.handle.isValid:
    yield startHeWalker.goToVertex
    var walker = startHeWalker.goPrev.goOpp
    
    while walker.handle != startHeWalker.handle:
      yield startHeWalker.goToVertex
      walker = walker.goPrev.goOpp  
    
iterator circulateFaces*(walker: VertexWalker): FaceWalker =
  var startHeWalker = walker.goOutHalfedge
  if startHeWalker.handle.isValid:
    yield startHeWalker.goFace
    var walker = startHeWalker.goPrev.goOpp
    
    while walker.handle != startHeWalker.handle:
      yield startHeWalker.goFace
      walker = walker.goPrev.goOpp  

iterator circulateOutEdges*(walker: VertexWalker): HalfedgeWalker =
  var startHeWalker = walker.goOutHalfedge
  if startHeWalker.handle.isValid:
    yield startHeWalker
    var walker = startHeWalker.goPrev.goOpp
    
    while walker.handle != startHeWalker.handle:
      yield startHeWalker
      walker = walker.goPrev.goOpp  

iterator circulateInEdges*(walker: VertexWalker): HalfedgeWalker =
  var startHeWalker = walker.goOutHalfedge
  if startHeWalker.handle.isValid:
    yield startHeWalker.goOpp
    var walker = startHeWalker.goPrev.goOpp
    
    while walker.handle != startHeWalker.handle:
      yield startHeWalker.goOpp
      walker = walker.goPrev.goOpp
      
iterator circulateEdges*(walker: VertexWalker): EdgeWalker =
  var startHeWalker = walker.goOutHalfedge
  if startHeWalker.handle.isValid:
    yield startHeWalker.goEdge
    var walker = startHeWalker.goPrev.goOpp
    
    while walker.handle != startHeWalker.handle:
      yield startHeWalker.goEdge
      walker = walker.goPrev.goOpp
      
#### face circulators ####

iterator circulateInHalfedges*(walker: FaceWalker): HalfedgeWalker =
  let startHeWalker = walker.goHalfedge
  yield startHeWalker
  var walker = startHeWalker.goNext
  while walker.handle != startHeWalker.handle:
    yield walker
    walker = walker.goNext

iterator circulateOutHalfedges*(walker: FaceWalker): HalfedgeWalker =
  let startHeWalker = walker.goHalfedge
  yield startHeWalker.goOpp
  var walker = startHeWalker.goNext
  while walker.handle != startHeWalker.handle:
    yield walker.goOpp
    walker = walker.goNext

iterator circulateEdges*(walker: FaceWalker): EdgeWalker =
  let startHeWalker = walker.goHalfedge
  yield startHeWalker.goEdge
  var walker = startHeWalker.goNext
  while walker.handle != startHeWalker.handle:
    yield walker.goEdge
    walker = walker.goNext
    
iterator circulateFaces*(walker: FaceWalker): FaceWalker =
  let startHeWalker = walker.goHalfedge
  yield startHeWalker.goOpp.goFace
  var walker = startHeWalker.goNext
  while walker.handle != startHeWalker.handle:
    yield walker.goOpp.goFace
    walker = walker.goNext

iterator circulateFaces*(walker: FaceWalker): VertexWalker =
  let startHeWalker = walker.goHalfedge
  yield startHeWalker.goToVertex
  var walker = startHeWalker.goNext
  while walker.handle != startHeWalker.handle:
    yield walker.goToVertex
    walker = walker.goNext

# basically everything are pairs

iterator circulateFaces*(walker: EdgeWalker): FaceWalker =
  yield walker.goHalfedge.goFace
  yield walker.goHalfedge.goOpp.goFace

iterator circulateVertices*(walker: EdgeWalker): VertexWalker =
  yield walker.goHalfedge.goToVertex
  yield walker.goHalfedge.goFromVertex

iterator circulateHalfedges*(walker: EdgeWalker): HalfedgeWalker =
  yield walker.goHalfedge
  yield walker.goHalfedge.goOpp
