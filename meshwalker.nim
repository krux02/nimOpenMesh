import openmesh

type
  BaseMesh_HalfedgeWalker = object
    mesh: ptr BaseMesh
    handle: HalfedgeHandle

  BaseMesh_VertexWalker = object
    mesh: ptr BaseMesh
    handle: VertexHandle

  BaseMesh_FaceWalker = object
    mesh: ptr BaseMesh
    handle: FaceHandle

  BaseMesh_EdgeWalker = object
    mesh: ptr BaseMesh
    handle: EdgeHandle

proc connectivity(walker: BaseMesh_HalfedgeWalker): Halfedge =
  walker.mesh.edges[walker.handle.int div 2][walker.handle.int and 1]

proc connectivity(walker: BaseMesh_VertexWalker): Vertex =
  walker.mesh.vertices[walker.handle.int]

proc connectivity(walker: BaseMesh_FaceWalker): Face =
  walker.mesh.faces[walker.handle.int]

proc connectivity(walker: BaseMesh_EdgeWalker): Edge =
  walker.mesh.edges[walker.handle.int]
  
template walkerMethods(VertexWalkerType, FaceWalkerType, EdgeWalkerType, HalfedgeWalkerType: typedesc) =
    
  proc goNext*(walker: HalfedgeWalkerType): HalfedgeWalkerType =
    result.mesh = walker.mesh
    result.handle = walker.connectivity.next_halfedge_handle
    
  proc goPrev*(walker: HalfedgeWalkerType): HalfedgeWalkerType =
    result.mesh = walker.mesh
    result.handle = walker.connectivity.prev_halfedge_handle
    
  proc goOpp*(walker: HalfedgeWalkerType): HalfedgeWalkerType =
    result.mesh = walker.mesh
    result.handle = HalfedgeHandle(walker.handle.int xor 1)
    
  proc goToVertex*(walker: HalfedgeWalkerType): VertexWalkerType =
    result.mesh = walker.mesh
    result.handle = walker.connectivity.vertex_handle
    
  proc goFromVertex*(walker: HalfedgeWalkerType): VertexWalkerType =
    walker.goOpp.goToVertex
    
  proc goFace*(walker: HalfedgeWalkerType): FaceWalkerType =
    result.mesh = walker.mesh
    result.handle = walker.connectivity.face_handle
    
  proc goEdge*(walker: HalfedgeWalkerType): EdgeWalkerType =
    result.mesh = walker.mesh
    result.handle = EdgeHandle(walker.handle.int div 2)
    
  proc goOutHalfedge*(walker: VertexWalkerType): HalfedgeWalkerType =
    result.mesh   = walker.mesh
    result.handle = walker.connectivity.out_halfedge_handle
    
  proc goHalfedge*(walker: FaceWalkerType): HalfedgeWalkerType =
    result.mesh   = walker.mesh
    result.handle = walker.connectivity.halfedge_handle
    
  proc goHalfedge*(walker: EdgeWalkerType): HalfedgeWalkerType =
    result.mesh   = walker.mesh
    result.handle = HalfedgeHandle(walker.handle.int * 2)

walkerMethods(BaseMesh_VertexWalker, BaseMesh_FaceWalker, BaseMesh_EdgeWalker, BaseMesh_HalfedgeWalker)
    
#### vertex circulators ####

iterator circulateVertices*(walker: BaseMesh_VertexWalker): BaseMesh_VertexWalker =
  var startHeWalker = walker.goOutHalfedge
  
  if startHeWalker.handle.isValid:
    yield startHeWalker.goToVertex
    var walker = startHeWalker.goPrev.goOpp
    
    while walker.handle != startHeWalker.handle:
      yield startHeWalker.goToVertex
      walker = walker.goPrev.goOpp  
    
iterator circulateFaces*(walker: BaseMesh_VertexWalker): BaseMesh_FaceWalker =
  var startHeWalker = walker.goOutHalfedge
  if startHeWalker.handle.isValid:
    yield startHeWalker.goFace
    var walker = startHeWalker.goPrev.goOpp
    
    while walker.handle != startHeWalker.handle:
      yield startHeWalker.goFace
      walker = walker.goPrev.goOpp  

iterator circulateOutEdges*(walker: BaseMesh_VertexWalker): BaseMesh_HalfedgeWalker =
  var startHeWalker = walker.goOutHalfedge
  if startHeWalker.handle.isValid:
    yield startHeWalker
    var walker = startHeWalker.goPrev.goOpp
    
    while walker.handle != startHeWalker.handle:
      yield startHeWalker
      walker = walker.goPrev.goOpp  

iterator circulateInEdges*(walker: BaseMesh_VertexWalker): BaseMesh_HalfedgeWalker =
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
