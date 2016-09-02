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
    #vprops, hprops, eprops, fprops, mprops: PropertyContainer
    vertices* : seq[Vertex]
    edges*    : seq[Edge]
    faces*    : seq[Face]

proc isValid*(handle: VertexHandle | HalfedgeHandle | EdgeHandle | FaceHandle): bool =
  handle.int >= 0
    
proc `[]`*(mesh: BaseMesh, handle: VertexHandle): Vertex =
  mesh.vertices[handle.int]
    
proc `[]`*(mesh: BaseMesh, handle: HalfedgeHandle): Halfedge =
  mesh.edges[handle.int div 2][handle.int and 1]

proc `[]`*(mesh: BaseMesh, handle: EdgeHandle): Edge =
  mesh.edges[handle.int]

proc `[]`*(mesh: BaseMesh, handle: FaceHandle): Face =
  mesh.faces[handle.int]

proc `[]`*[T](mesh: ptr BaseMesh, handle: T): auto = mesh[][handle]
