import macros

template walkerMethodsTemplate(VertexRefType, FaceRefType, EdgeRefType, HalfedgeRefType: typedesc) =

  proc connectivity(halfedge: HalfedgeRefType): Halfedge =
    halfedge.mesh.edges[halfedge.handle.int div 2][halfedge.handle.int and 1]

  proc connectivity(vertes: VertexRefType): Vertex =
    vertes.mesh.vertices[vertes.handle.int]

  proc connectivity(face: FaceRefType): Face =
    face.mesh.faces[face.handle.int]

  proc connectivity(edge: EdgeRefType): Edge =
    edge.mesh.edges[edge.handle.int]

  ## movement methods ##
    
  proc goNext*(halfedge: HalfedgeRefType): HalfedgeRefType =
    result.mesh = halfedge.mesh
    result.handle = halfedge.connectivity.next_halfedge_handle
    
  proc goPrev*(halfedge: HalfedgeRefType): HalfedgeRefType =
    result.mesh = halfedge.mesh
    result.handle = halfedge.connectivity.prev_halfedge_handle
    
  proc goOpp*(halfedge: HalfedgeRefType): HalfedgeRefType =
    result.mesh = halfedge.mesh
    result.handle = HalfedgeHandle(halfedge.handle.int xor 1)
    
  proc goToVertex*(halfedge: HalfedgeRefType): VertexRefType =
    result.mesh = halfedge.mesh
    result.handle = halfedge.connectivity.vertex_handle
    
  proc goFromVertex*(halfedge: HalfedgeRefType): VertexRefType =
    halfedge.goOpp.goToVertex
    
  proc goFace*(halfedge: HalfedgeRefType): FaceRefType =
    result.mesh = halfedge.mesh
    result.handle = halfedge.connectivity.face_handle
    
  proc goEdge*(halfedge: HalfedgeRefType): EdgeRefType =
    result.mesh = halfedge.mesh
    result.handle = EdgeHandle(halfedge.handle.int div 2)
    
  proc goOutHalfedge*(vertex: VertexRefType): HalfedgeRefType =
    result.mesh   = vertex.mesh
    result.handle = vertex.connectivity.out_halfedge_handle
    
  proc goHalfedge*(face: FaceRefType): HalfedgeRefType =
    result.mesh   = face.mesh
    result.handle = face.connectivity.halfedge_handle
    
  proc goHalfedge*(edge: EdgeRefType): HalfedgeRefType =
    result.mesh   = edge.mesh
    result.handle = HalfedgeHandle(edge.handle.int * 2)

  ## circulators ##
    
  iterator circulateVertices*(vertex: VertexRefType): VertexRefType =
    let startHalfedge = vertex.goOutHalfedge
    if startHalfedge.handle.isValid:
      yield startHalfedge.goToVertex
      var walker = startHalfedge.goPrev.goOpp
      
      while walker.handle != startHalfedge.handle:
        yield walker.goToVertex
        walker = walker.goPrev.goOpp  
      
  iterator circulateFaces*(vertex: VertexRefType): FaceRefType =
    let startHalfedge = vertex.goOutHalfedge
    if startHalfedge.handle.isValid:
      yield startHalfedge.goFace
      var walker = startHalfedge.goPrev.goOpp
      
      while walker.handle != startHalfedge.handle:
        yield walker.goFace
        walker = walker.goPrev.goOpp  
  
  iterator circulateOutHalfedges*(vertex: VertexRefType): HalfedgeRefType =
    let startHalfedge = vertex.goOutHalfedge
    if startHalfedge.handle.isValid:
      yield startHalfedge
      var walker = startHalfedge.goPrev.goOpp
      
      while walker.handle != startHalfedge.handle:
        yield walker
        walker = walker.goPrev.goOpp  
  
  iterator circulateInHalfedges*(vertex: VertexRefType): HalfedgeRefType =
    let startHalfedge = vertex.goOutHalfedge
    if startHalfedge.handle.isValid:
      yield startHalfedge.goOpp
      var walker = startHalfedge.goPrev.goOpp
      
      while walker.handle != startHalfedge.handle:
        yield walker.goOpp
        walker = walker.goPrev.goOpp
        
  iterator circulateEdges*(vertex: VertexRefType): EdgeRefType =
    let startHalfedge = vertex.goOutHalfedge
    if startHalfedge.handle.isValid:
      yield startHalfedge.goEdge
      var walker = startHalfedge.goPrev.goOpp
      
      while walker.handle != startHalfedge.handle:
        yield walker.goEdge
        walker = walker.goPrev.goOpp
        
  #### face circulators ####
  
  iterator circulateInHalfedges*(face: FaceRefType): HalfedgeRefType =
    let startHalfedge = face.goHalfedge
    yield startHalfedge
    var walker = startHalfedge.goNext
    while walker.handle != startHalfedge.handle:
      yield walker
      walker = walker.goNext
  
  iterator circulateOutHalfedges*(face: FaceRefType): HalfedgeRefType =
    let startHalfedge = face.goHalfedge
    yield startHalfedge.goOpp
    var walker = startHalfedge.goNext
    while walker.handle != startHalfedge.handle:
      yield walker.goOpp
      walker = walker.goNext
  
  iterator circulateEdges*(face: FaceRefType): EdgeRefType =
    let startHalfedge = face.goHalfedge
    yield startHalfedge.goEdge
    var walker = startHalfedge.goNext
    while walker.handle != startHalfedge.handle:
      yield walker.goEdge
      walker = walker.goNext
      
  iterator circulateFaces*(face: FaceRefType): FaceRefType =
    let startHalfedge = face.goHalfedge
    yield startHalfedge.goOpp.goFace
    var walker = startHalfedge.goNext
    while walker.handle != startHalfedge.handle:
      yield walker.goOpp.goFace
      walker = walker.goNext
  
  iterator circulateVertices*(face: FaceRefType): VertexRefType =
    let startHalfedge = face.goHalfedge
    yield startHalfedge.goToVertex
    var walker = startHalfedge.goNext
    while walker.handle != startHalfedge.handle:
      yield walker.goToVertex
      walker = walker.goNext
  
  # basically everything are pairs
  
  iterator circulateFaces*(edge: EdgeRefType): FaceRefType =
    yield edge.goHalfedge.goFace
    yield edge.goHalfedge.goOpp.goFace
  
  iterator circulateVertices*(edge: EdgeRefType): VertexRefType =
    yield edge.goHalfedge.goToVertex
    yield edge.goHalfedge.goFromVertex
  
  iterator circulateHalfedges*(edge: EdgeRefType): HalfedgeRefType =
    yield edge.goHalfedge
    yield edge.goHalfedge.goOpp

  proc valence(vertex: VertexRefType): int =
    for halfedge in vertex.circulateOutHalfedges:
      result += 1

  proc valence(face: FaceRefType): int =
    for halfedge in face.circulateInHalfedges:
      result += 1

macro walkerMethods*(MeshType: typed): untyped =
  let
    HalfedgeRefType = ident($MeshType.symbol & "_HalfedgeRef")
    VertexRefType   = ident($MeshType.symbol & "_VertexRef")
    FaceRefType     = ident($MeshType.symbol & "_FaceRef")
    EdgeRefType     = ident($MeshType.symbol & "_EdgeRef")

  result = quote do:
    walkerMethodsTemplate(`VertexRefType`, `FaceRefType`,
                          `EdgeRefType`,   `HalfedgeRefType`)
