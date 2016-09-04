import macros

template walkerMethodsTemplate(VertexWalkerType, FaceWalkerType, EdgeWalkerType, HalfedgeWalkerType: typedesc) =

  proc connectivity(walker: HalfedgeWalkerType): Halfedge =
    walker.mesh.edges[walker.handle.int div 2][walker.handle.int and 1]

  proc connectivity(walker: VertexWalkerType): Vertex =
    walker.mesh.vertices[walker.handle.int]

  proc connectivity(walker: FaceWalkerType): Face =
    walker.mesh.faces[walker.handle.int]

  proc connectivity(walker: EdgeWalkerType): Edge =
    walker.mesh.edges[walker.handle.int]

  ## movement methods ##
    
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

  ## circulators ##
    
  iterator circulateVertices*(walker: VertexWalkerType): VertexWalkerType =
    var startHeWalker = walker.goOutHalfedge
    
    if startHeWalker.handle.isValid:
      yield startHeWalker.goToVertex
      var walker = startHeWalker.goPrev.goOpp
      
      while walker.handle != startHeWalker.handle:
        yield startHeWalker.goToVertex
        walker = walker.goPrev.goOpp  
      
  iterator circulateFaces*(walker: VertexWalkerType): FaceWalkerType =
    var startHeWalker = walker.goOutHalfedge
    if startHeWalker.handle.isValid:
      yield startHeWalker.goFace
      var walker = startHeWalker.goPrev.goOpp
      
      while walker.handle != startHeWalker.handle:
        yield startHeWalker.goFace
        walker = walker.goPrev.goOpp  
  
  iterator circulateOutEdges*(walker: VertexWalkerType): HalfedgeWalkerType =
    var startHeWalker = walker.goOutHalfedge
    if startHeWalker.handle.isValid:
      yield startHeWalker
      var walker = startHeWalker.goPrev.goOpp
      
      while walker.handle != startHeWalker.handle:
        yield startHeWalker
        walker = walker.goPrev.goOpp  
  
  iterator circulateInEdges*(walker: VertexWalkerType): HalfedgeWalkerType =
    var startHeWalker = walker.goOutHalfedge
    if startHeWalker.handle.isValid:
      yield startHeWalker.goOpp
      var walker = startHeWalker.goPrev.goOpp
      
      while walker.handle != startHeWalker.handle:
        yield startHeWalker.goOpp
        walker = walker.goPrev.goOpp
        
  iterator circulateEdges*(walker: VertexWalkerType): EdgeWalkerType =
    var startHeWalker = walker.goOutHalfedge
    if startHeWalker.handle.isValid:
      yield startHeWalker.goEdge
      var walker = startHeWalker.goPrev.goOpp
      
      while walker.handle != startHeWalker.handle:
        yield startHeWalker.goEdge
        walker = walker.goPrev.goOpp
        
  #### face circulators ####
  
  iterator circulateInHalfedges*(arg: FaceWalkerType): HalfedgeWalkerType =
    let startHeWalker = arg.goHalfedge
    yield startHeWalker
    var walker = startHeWalker.goNext
    while walker.handle != startHeWalker.handle:
      yield walker
      walker = walker.goNext
  
  iterator circulateOutHalfedges*(arg: FaceWalkerType): HalfedgeWalkerType =
    let startHeWalker = arg.goHalfedge
    yield startHeWalker.goOpp
    var walker = startHeWalker.goNext
    while walker.handle != startHeWalker.handle:
      yield walker.goOpp
      walker = walker.goNext
  
  iterator circulateEdges*(arg: FaceWalkerType): EdgeWalkerType =
    let startHeWalker = arg.goHalfedge
    yield startHeWalker.goEdge
    var walker = startHeWalker.goNext
    while walker.handle != startHeWalker.handle:
      yield walker.goEdge
      walker = walker.goNext
      
  iterator circulateFaces*(arg: FaceWalkerType): FaceWalkerType =
    let startHeWalker = arg.goHalfedge
    yield startHeWalker.goOpp.goFace
    var walker = startHeWalker.goNext
    while walker.handle != startHeWalker.handle:
      yield walker.goOpp.goFace
      walker = walker.goNext
  
  iterator circulateVertices*(arg: FaceWalkerType): VertexWalkerType =
    let startHeWalker = arg.goHalfedge
    yield startHeWalker.goToVertex
    var walker = startHeWalker.goNext
    while walker.handle != startHeWalker.handle:
      yield walker.goToVertex
      walker = walker.goNext
  
  # basically everything are pairs
  
  iterator circulateFaces*(walker: EdgeWalkerType): FaceWalkerType =
    yield walker.goHalfedge.goFace
    yield walker.goHalfedge.goOpp.goFace
  
  iterator circulateVertices*(walker: EdgeWalkerType): VertexWalkerType =
    yield walker.goHalfedge.goToVertex
    yield walker.goHalfedge.goFromVertex
  
  iterator circulateHalfedges*(walker: EdgeWalkerType): HalfedgeWalkerType =
    yield walker.goHalfedge
    yield walker.goHalfedge.goOpp


macro walkerMethods*(MeshType: typed): stmt =
  let
    HalfedgeWalkerType = ident($MeshType.symbol & "_HalfedgeWalker")
    VertexWalkerType   = ident($MeshType.symbol & "_VertexWalker")
    FaceWalkerType     = ident($MeshType.symbol & "_FaceWalker")
    EdgeWalkerType     = ident($MeshType.symbol & "_EdgeWalker")

  result = quote do:
    walkerMethodsTemplate(`VertexWalkerType`, `FaceWalkerType`,
                          `EdgeWalkerType`,   `HalfedgeWalkerType`)
