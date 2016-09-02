#this is supposed to be a nim port of open mesh.

type
  VertexHandle*      = distinct int
  HalfedgeHandle*    = distinct int
  EdgeHandle*        = distinct int
  FaceHandle*        = distinct int

  Halfedge* = object
    face_handle*:          FaceHandle  
    vertex_handle*:        VertexHandle
    next_halfedge_handle*: HalfedgeHandle
    prev_halfedge_handle*: HalfedgeHandle

  Edge* = array[2, Halfedge]

  Vertex* = object
    out_halfedge_handle*: HalfedgeHandle

  Face*   = object
    halfedge_handle*: HalfedgeHandle

  BaseMesh* = object
    vertices* : seq[Vertex]
    edges*    : seq[Edge]
    faces*    : seq[Face]

template handleProcs(HandleType: typedesc) =
  proc `==`*(handleA, handleB: HandleType): bool =
    handleA == handleB

  proc isValid*(handle: HandleType): bool =
    handle.int >= 0

handleProcs(VertexHandle)
handleProcs(HalfedgeHandle)
handleProcs(EdgeHandle)
handleProcs(FaceHandle)
