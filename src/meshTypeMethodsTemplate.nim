import math, macros

proc angle(cos_angle, sin_angle: float32): float32 =
  # sanity checks - otherwise acos will return nan
  result = arccos(clamp(cos_angle, -1, 1))
  if sin_angle < 0:
    result *= -1

template meshRefTypesTemplate*(MeshType: typedesc): untyped =

  template HalfedgeRef*(_ : typedesc[MeshType]): untyped =
    GHalfedgeRef[MeshType]

  template EdgeRef*(_ : typedesc[MeshType]): untyped =
    GEdgeRef[MeshType]

  template FaceRef*(_ : typedesc[MeshType]): untyped =
    GFaceRef[MeshType]

  template VertexRef*(_ : typedesc[MeshType]): untyped =
    GVertexRef[MeshType]


template meshTypeMethodsTemplate*(MeshType: typedesc): untyped =
  # Pairs of handle and pointer to the mesh.  Basically they are
  # pointer types that point into an index of a `seq` and don't break
  # when the seq resizes.

  template makeRef*(m: var MeshType; h: VertexHandle): untyped = MeshType.VertexRef(mesh: m.addr, handle: h)
  template makeRef*(m: var MeshType; h: FaceHandle): untyped = MeshType.FaceRef(mesh: m.addr, handle: h)
  template makeRef*(m: var MeshType; h: EdgeHandle): untyped = MeshType.EdgeRef(mesh: m.addr, handle: h)
  template makeRef*(m: var MeshType; h: HalfedgeHandle): untyped = MeshType.HalfedgeRef(mesh: m.addr, handle: h)

  iterator vertexRefs*(mesh : var MeshType) : MeshType.VertexRef =
    var vertexRef : MeshType.VertexRef
    vertexRef.mesh = mesh.addr
    for i in 0 .. high(mesh.vertices):
      vertexRef.handle = VertexHandle(i)
      yield(vertexRef)

  iterator faceRefs*(mesh : var MeshType) : MeshType.FaceRef =
    var faceRef : MeshType.FaceRef
    faceRef.mesh = mesh.addr
    for i in 0 .. high(mesh.faces):
      faceRef.handle = FaceHandle(i)
      yield(faceRef)

  iterator edgeRefs*(mesh : var MeshType) : MeshType.EdgeRef =
    var edgeRef : MeshType.EdgeRef
    edgeRef.mesh = mesh.addr
    for i in 0 .. high(mesh.edges):
      edgeRef.handle = EdgeHandle(i)
      yield(edgeRef)

  iterator halfedgeRefs*(mesh : var MeshType) : MeshType.HalfedgeRef =
    var halfedgeRef : MeshType.HalfedgeRef
    halfedgeRef.mesh = mesh.addr
    for i in 0 .. high(mesh.edges):
      halfedgeRef.handle = HalfedgeHandle(i*2)
      yield(halfedgeRef)
      halfedgeRef.handle = HalfedgeHandle(i*2+1)
      yield(halfedgeRef)

  # TODO maybe not export
  proc connectivity*(mesh: var MeshType, handle: HalfedgeHandle): var Halfedge =
    mesh.edges[handle.int div 2][handle.int and 1]
  proc connectivity*(mesh: var MeshType, handle: VertexHandle): var Vertex =
    mesh.vertices[handle.int]
  proc connectivity*(mesh: var MeshType, handle: FaceHandle): var Face =
    mesh.faces[handle.int]

  proc connectivity*(halfedge: MeshType.HalfedgeRef): var Halfedge =
    halfedge.mesh.edges[halfedge.handle.int div 2][halfedge.handle.int and 1]

  proc connectivity*(vertes: MeshType.VertexRef): var Vertex =
    vertes.mesh.vertices[vertes.handle.int]

  proc connectivity*(face: MeshType.FaceRef): var Face =
    face.mesh.faces[face.handle.int]

  # is never used
  #proc connectivity(edge: MeshType.EdgeRef): Edge =
  #  edge.mesh.edges[edge.handle.int]

  ## movement methods ##

  proc goNext*(halfedge: MeshType.HalfedgeRef): MeshType.HalfedgeRef =
    result.mesh = halfedge.mesh
    result.handle = halfedge.connectivity.next_halfedge_handle

  proc goPrev*(halfedge: MeshType.HalfedgeRef): MeshType.HalfedgeRef =
    result.mesh = halfedge.mesh
    result.handle = halfedge.connectivity.prev_halfedge_handle

  proc goOpp*(halfedge: MeshType.HalfedgeRef): MeshType.HalfedgeRef =
    result.mesh = halfedge.mesh
    result.handle = HalfedgeHandle(halfedge.handle.int xor 1)

  proc goToVertex*(halfedge: MeshType.HalfedgeRef): MeshType.VertexRef =
    result.mesh = halfedge.mesh
    result.handle = halfedge.connectivity.vertex_handle

  proc goFromVertex*(halfedge: MeshType.HalfedgeRef): MeshType.VertexRef =
    halfedge.goOpp.goToVertex

  proc goFace*(halfedge: MeshType.HalfedgeRef): MeshType.FaceRef =
    result.mesh = halfedge.mesh
    result.handle = halfedge.connectivity.face_handle

  proc goEdge*(halfedge: MeshType.HalfedgeRef): MeshType.EdgeRef =
    result.mesh = halfedge.mesh
    result.handle = EdgeHandle(halfedge.handle.int div 2)

  proc goOutHalfedge*(vertex: MeshType.VertexRef): MeshType.HalfedgeRef =
    result.mesh   = vertex.mesh
    result.handle = vertex.connectivity.out_halfedge_handle

  proc goHalfedge*(face: MeshType.FaceRef): MeshType.HalfedgeRef =
    result.mesh   = face.mesh
    result.handle = face.connectivity.halfedge_handle

  proc goHalfedge*(edge: MeshType.EdgeRef): MeshType.HalfedgeRef =
    result.mesh   = edge.mesh
    result.handle = HalfedgeHandle(edge.handle.int * 2)

  ## circulators ##

  iterator circulateVertices*(vertex: MeshType.VertexRef): MeshType.VertexRef =
    let startHalfedge = vertex.goOutHalfedge
    if startHalfedge.handle.isValid:
      yield startHalfedge.goToVertex
      var walker = startHalfedge.goPrev.goOpp

      while walker.handle != startHalfedge.handle:
        yield walker.goToVertex
        walker = walker.goPrev.goOpp

  iterator circulateFaces*(vertex: MeshType.VertexRef): MeshType.FaceRef =
    let startHalfedge = vertex.goOutHalfedge
    if startHalfedge.handle.isValid:
      yield startHalfedge.goFace
      var walker = startHalfedge.goPrev.goOpp

      while walker.handle != startHalfedge.handle:
        yield walker.goFace
        walker = walker.goPrev.goOpp

  iterator circulateOutHalfedges*(vertex: MeshType.VertexRef): MeshType.HalfedgeRef =
    let startHalfedge = vertex.goOutHalfedge
    if startHalfedge.handle.isValid:
      yield startHalfedge
      var walker = startHalfedge.goPrev.goOpp

      while walker.handle != startHalfedge.handle:
        yield walker
        walker = walker.goPrev.goOpp

  iterator circulateInHalfedges*(vertex: MeshType.VertexRef): MeshType.HalfedgeRef =
    let startHalfedge = vertex.goOutHalfedge
    if startHalfedge.handle.isValid:
      yield startHalfedge.goOpp
      var walker = startHalfedge.goPrev.goOpp

      while walker.handle != startHalfedge.handle:
        yield walker.goOpp
        walker = walker.goPrev.goOpp

  iterator circulateEdges*(vertex: MeshType.VertexRef): MeshType.EdgeRef =
    let startHalfedge = vertex.goOutHalfedge
    if startHalfedge.handle.isValid:
      yield startHalfedge.goEdge
      var walker = startHalfedge.goPrev.goOpp

      while walker.handle != startHalfedge.handle:
        yield walker.goEdge
        walker = walker.goPrev.goOpp

  #### face circulators ####

  iterator circulateInHalfedges*(face: MeshType.FaceRef): MeshType.HalfedgeRef =
    let startHalfedge = face.goHalfedge
    yield startHalfedge
    var walker = startHalfedge.goNext
    while walker.handle != startHalfedge.handle:
      yield walker
      walker = walker.goNext

  iterator circulateOutHalfedges*(face: MeshType.FaceRef): MeshType.HalfedgeRef =
    let startHalfedge = face.goHalfedge
    yield startHalfedge.goOpp
    var walker = startHalfedge.goNext
    while walker.handle != startHalfedge.handle:
      yield walker.goOpp
      walker = walker.goNext

  iterator circulateEdges*(face: MeshType.FaceRef): MeshType.EdgeRef =
    let startHalfedge = face.goHalfedge
    yield startHalfedge.goEdge
    var walker = startHalfedge.goNext
    while walker.handle != startHalfedge.handle:
      yield walker.goEdge
      walker = walker.goNext

  iterator circulateFaces*(face: MeshType.FaceRef): MeshType.FaceRef =
    let startHalfedge = face.goHalfedge
    yield startHalfedge.goOpp.goFace
    var walker = startHalfedge.goNext
    while walker.handle != startHalfedge.handle:
      yield walker.goOpp.goFace
      walker = walker.goNext

  iterator circulateVertices*(face: MeshType.FaceRef): MeshType.VertexRef =
    let startHalfedge = face.goHalfedge
    yield startHalfedge.goToVertex
    var walker = startHalfedge.goNext
    while walker.handle != startHalfedge.handle:
      yield walker.goToVertex
      walker = walker.goNext

  # basically everything are pairs

  iterator circulateFaces*(edge: MeshType.EdgeRef): MeshType.FaceRef =
    yield edge.goHalfedge.goFace
    yield edge.goHalfedge.goOpp.goFace

  iterator circulateVertices*(edge: MeshType.EdgeRef): MeshType.VertexRef =
    yield edge.goHalfedge.goToVertex
    yield edge.goHalfedge.goFromVertex

  iterator circulateHalfedges*(edge: MeshType.EdgeRef): MeshType.HalfedgeRef =
    yield edge.goHalfedge
    yield edge.goHalfedge.goOpp

  proc valence*(vertex: MeshType.VertexRef): int =
    for halfedge in vertex.circulateOutHalfedges:
      result += 1

  proc valence*(face: MeshType.FaceRef): int =
    for halfedge in face.circulateInHalfedges:
      result += 1

  proc find_halfedge(vertex1, vertex2: MeshType.VertexRef): MeshType.HalfedgeRef =
    ## Returs the Halfedge ref that points from `vertex1` to
    ## `vertex2`. If such halfedge does not exist, returns a ref with
    ## an invalid handle.
    assert(vertex1.mesh == vertex2.mesh)
    assert(vertex1.handle.is_valid() and vertex2.handle.is_valid())

    for it in vertex1.circulateOutHalfedges:
      if it.connectivity.vertex_handle == vertex2.handle:
        return it

    result.mesh = vertex1.mesh
    result.handle = InvalidHalfedgeHandle;

  proc is_boundary*(halfedge: MeshType.HalfedgeRef): bool =
    return not halfedge.goFace.handle.isValid

  proc is_boundary*(edge: MeshType.EdgeRef): bool =
    for halfedge in edge.circulateHalfedges:
      if halfedge.is_boundary:
        return true
    return false

  proc is_boundary*(vertex: MeshType.VertexRef): bool =
    ## Checks if the associated halfedge (which would on a boundary be the outside
    ## halfedge), is connected to a face. Which is equivalent, if the vertex is
    ## at the boundary of the mesh, as OpenMesh will make sure, that if there is a
    ## boundary halfedge at the vertex, the halfedge will be the one which is associated
    ## to the vertex.

    let heh = vertex.goOutHalfedge
    return heh.handle.is_invalid or heh.goFace.handle.is_invalid

  proc is_boundary*(face: MeshType.FaceRef, check_vertex: bool = false): bool =
    ## Is face ``faceRef`` at boundary, i.e. is one of its edges (or vertices)
    ## a boundary edge?
    ##
    ## If ``check_vertex`` is true, the corner vertices of the face are checked, too

    # TODO this is ported like this from open mesh, but wouldn't it be faster when only out halfedges are checked?
    for edge in face.circulateEdges: # faceRef.circulateOutHalfedges
      if edge.is_boundary:
        return true

    if check_vertex:
      for vertex in face.circulateVertices:
        if vertex.is_boundary:
          return true

    return false

  proc is_not_boundary*(face: MeshType.FaceRef, check_vertex: bool = false): bool =
    not is_boundary(face, check_vertex)

  proc is_not_boundary*(vertex: MeshType.VertexRef): bool =
    not is_boundary(vertex)

  proc is_not_boundary*(edge: MeshType.EdgeRef): bool =
    not is_boundary(edge)

  proc is_not_boundary*(halfedge: MeshType.HalfedgeRef): bool =
    not is_boundary(halfedge)

  proc calc_face_normal(face: MeshType.FaceRef): MeshType.vertexPropertyType(point) =
    assert(face.goHalfedge.handle.isValid)
    # http://www.opengl.org/wiki/Calculating_a_Surface_Normal

    for halfedge in face.circulateInHalfedges:
      let
        pointA = halfedge.goFromVertex.propPoint()
        pointB = halfedge.goToVertex.propPoint()

        a = pointA - pointB
        b = pointA + pointB

      result[0] = result[0] + a[1] * b[2]
      result[1] = result[1] + a[2] * b[0]
      result[2] = result[2] + a[0] * b[1]

    let length = result.length

    if length != 0:
      result = result / length


  proc getNormal(face: MeshType.FaceRef) : MeshType.vertexPropertyType(point) =
    ## Either returns the normal property when available, or calculates it, when the property is not available
    when hasFaceProperty(MyMeshType, normal):
      return face.propNormal
    else:
      return face.calc_face_normal

  proc is_estimated_feature_edge*(halfedge: MeshType.HalfedgeRef, feature_angle: float): bool =
    ## uses face normal attribute, when available
    let edge = halfedge.goEdge

    when hasEdgeProperty(MyMeshType,status):
      if edge.status.feature:
        return true;

    if edge.is_boundary:
      return false;

    # compute angle between faces
    let
      faceNormal0 = halfedge.goFace.getNormal
      facenormal1 = halfedge.goOpp.goFace.getNormal

    return dot(faceNormal0,faceNormal1) < cos(feature_angle)

  proc is_not_estimated_feature_edge*(halfedge: MeshType.HalfedgeRef, feature_angle: float): bool =
    not is_estimated_feature_edge(halfedge, feature_angle)

  proc calc_edge_vector*(halfedge: MeshType.HalfedgeRef): auto =
    ## Calculates the edge vector as the difference of the
    ## the points defined by to_vertex_handle and from_vertex_handle
    halfedge.goToVertex.propPoint - halfedge.goFromVertex.propPoint

  proc calc_edge_vector*(edge: MeshType.EdgeRef): auto =
    ## Calculates the edge vector as the difference of the
    ## the points defined by to_vertex_handle and from_vertex_handle
    edge.goHalfedge.calc_edge_vector

  proc calc_edge_sqr_length*(halfedge: MeshType.HalfedgeRef): auto =
    let v = halfedge.calc_edge_vector
    return dot(v,v)

  proc calc_edge_sqr_length*(edge: MeshType.EdgeRef): auto =
    edge.goHalfedge.calc_edge_sqr_length

  proc calc_edge_length*(halfedge: MeshType.HalfedgeRef): auto =
    halfedge.calc_edge_sqr_length.sqrt

  proc calc_edge_length*(edge: MeshType.EdgeRef): auto =
    edge.goHalfedge.calc_edge_length

  proc calc_sector_vectors*(halfedge: MeshType.HalfedgeRef) : tuple[vec0, vec1: MeshType.vertexPropertyType(point)] =
    ## defines a consistent representation of a sector geometry:
    ## the halfedge _in_heh defines the sector orientation
    ## the vertex pointed by _in_heh defines the sector center
    ## vec0 and vec1 are resp. the first and the second vectors defining the sector
    result.vec0 = halfedge.goNext.calc_edge_vector # p2 - p1
    result.vec1 = halfedge.goOpp.calc_edge_vector  # p0 - p1

  proc calc_sector_normal_unnormalized*(halfedge: MeshType.HalfedgeRef) : MeshType.vertexPropertyType(point) =
    ## calculates the normal (non-normalized) of the face sector defined by
    ## the angle <(_in_heh,next_halfedge(_in_heh))
    let (vec0, vec1) = halfedge.calc_sector_vectors
    xyz(result) = cross(vec0.xyz, vec1.xyz) # (p2-p1)^(p0-p1)

  bind angle

  proc calc_dihedral_angle*(halfedge: MeshType.HalfedgeRef) : MeshType.vertexPropertyType(point).T =
    if halfedge.goEdge.is_boundary:
      return 0


    let
      n0 = halfedge.calc_sector_normal_unnormalized
      n1 = halfedge.goOpp.calc_sector_normal_unnormalized
      he = halfedge.calc_edge_vector
      denom = length(n0) * length(n1)

    if denom == 0:
      return 0

    var
      da_cos = dot(n0, n1) / denom
      da_sin_sign = dot(cross(n0.xyz, n1.xyz), he.xyz) #should be normalized, but we need only the sign

    return angle(da_cos, da_sin_sign)

  proc calc_dihedral_angle_fast*(halfedge: MeshType.HalfedgeRef): MeshType.vertexPropertyType(point).T =
    ## Calculates the dihedral angle on the halfedge.
    ## Attention Needs the Attributes::Normal attribute for faces.
    if halfedge.goEdge.is_boundary:
      return 0

    let
      n0 = halfedge.goFace.getNormal
      n1 = halfedge.goOpp.goFace.getNormal

      he = halfedge.calc_edge_vector
      da_cos = dot(n0, n1)
      da_sin_sign = dot(cross(n0.xyz, n1.xyz), he.xyz) #should be normalized, but we need only the sign

    return angle(da_cos, da_sin_sign)

  proc calc_face_centroid*(face: MeshType.FaceRef): MeshType.vertexPropertyType(point) =
    ## calculates the average of the vertices defining the face
    var valence: type(result[0])
    for vertex in face.circulateVertices:
      result += vertex.propPoint
      valence += 1

    result = result / valence

  proc calc_halfedge_normal*(inHalfedge: MeshType.HalfedgeRef, feature_angle: float64): MeshType.vertexPropertyType(point) =
    ## calculates the normal of the To vertex, but takes boundaries into consideration.
    ## this means that faces are not cut away

    ## Calculate normal vector for halfedge _heh.

    ## Face normals have to be computed first!

    ## Needs the Attributes::Normal attribute for faces and vertices.
    ## If the dihedral angle across this edge is greater than ``feature_angle``, the edge is considered as a feature edge (angle in radians).

    ## ..image:: images/halfedge_normal.svg


    if inHalfedge.is_boundary:
      return

    var halfedge = inHalfedge

    # collect CW face-handles
    while true:
      result += halfedge.goFace.getNormal
      halfedge = halfedge.goNext.goOpp

      if halfedge.handle == inHalfedge.handle or halfedge.is_boundary or halfedge.is_estimated_feature_edge(feature_angle):
        break

    # collect CCW face-handles
    if halfedge.handle != inHalfedge.handle and halfedge.is_not_estimated_feature_edge(feature_angle):
      halfedge = inHalfedge.goOpp

      if halfedge.is_not_boundary:
        while true:
          result += halfedge.goFace.getNormal
          halfedge = halfedge.goPrev.goOpp

          if halfedge.is_boundary or halfedge.is_estimated_feature_edge(feature_angle):
            break

      result = result.normalize


  proc calc_sector_angle*(halfedge: MeshType.HalfedgeRef): MeshType.vertexPropertyType(point).T =
    ## The vertex pointed by ``halfedge`` defines the sector center
    ## The angle will be calculated between the given halfedge and the next halfedge.
    ## Seen from the center vertex this will be the next halfedge in clockwise direction.
    ## NOTE: only boundary concave sectors are treated correctly, result can be zero


    let (v0,v1) = halfedge.calc_sector_vectors()
    let denom = v0.length * v1.length
    if denom == 0:
      return 0;
    let cos_a = dot(v0, v1) / denom

    if halfedge.is_boundary:
      # determine if the boundary sector is concave or convex
      let
        face = halfedge.goOpp.goFace
        fn = face.calc_face_normal # this normal is (for convex fh) OK
        sign_a = dot(cross(v0.xyz, v1.xyz), fn.xyz)

      return angle(cos_a, sign_a)

    else:
      return arccos(clamp(cos_a,-1,1));


  proc calc_sector_area*(halfedge: MeshType.HalfedgeRef): MeshType.vertexPropertyType(point).T =
    ## calculates the area of the face sector defined by
    ## the angle ``(inHalfedge,inHalfedge.goNext)``
    ## NOTE: special cases (e.g. concave sectors) are not handled correctly
    let sector_normal = halfedge.calc_sector_normal_unnormalized
    return sector_normal.length / 2

  proc calc_vertex_normal_fast*(vertex: MeshType.VertexRef): MeshType.vertexPropertyType(point) =
    for face in vertex.circulateFaces:
      result = result + face.getNormal

  proc calc_vertex_normal*(vertex: MeshType.VertexRef): MeshType.vertexPropertyType(point) =
    result = vertex.calc_vertex_normal_fast
    let length_sq = dot(result, result) # squared length
    if length_sq != 0:
      result = result / sqrt(length_sq)

  import loopschememask

  proc calc_vertex_normal_loop*(vertex: MeshType.VertexRef): MeshType.vertexPropertyType(point) =

    #static const LoopSchemeMaskDouble& loop_scheme_mask__ =
    #                LoopSchemeMaskDoubleSingleton::Instance();

    let valence = vertex.valence

    var
      t_w: type(vertex.propPoint)
      t_v: type(vertex.propNormal)

    var i = 0
    for vertex in vertex.circulateVertices:
      t_v = t_v + loopschememask.tang0_weight(valence, i) * vertex.propNormal
      t_w = t_w + loopschememask.tang1_weight(valence, i) * vertex.propPoint
      i += 1

    # hack: should be cross(t_v, t_w), but then the normals are reversed?
    result.xyz() = cross(t_w.xyz, t_v.xyz)

  proc calc_vertex_normal_correct*(vertex: MeshType.VertexRef): MeshType.vertexPropertyType(point) =
    ## there is no correct way to calculate the vertex normal
    if vertex.goOutHalfedge.handle.is_invalid:
      # dont crash on isolated vertices
      return

    var in_he_vec = vertex.goOutHalfedge.goOpp.calc_edge_vector

    for inHalfedge in vertex.circulateInHalfedges:
      # calculates the sector normal defined by ``halfedge`` and adds it to ``result``
      if inHalfedge.is_boundary:
        continue

      let outHalfedge = inHalfedge.goNext
      let out_he_vec = outHalfedge.calc_edge_vector

      result.xyz() += cross(in_he_vec.xyz, out_he_vec.xyz); # sector area is taken into account

      in_he_vec = - out_he_vec; # change the orientation

  proc new_edge(mesh: var MeshType, startVh,endVh: VertexHandle): HalfedgeHandle {.inline.} =
    let he = mesh.addEdge.goHalfedge
    he.connectivity.vertex_handle = endVh
    he.goOpp.connectivity.vertex_handle = startVh
    result = he.handle

  proc adjust_outgoing_halfedge*(arg: MeshType.VertexRef) =
    for it in arg.circulateOutHalfedges:
      if it.isBoundary:
        arg.connectivity.out_halfedge_handle = it.handle
        break

  proc addFace*(mesh: var MeshType; vertex_handles: openArray[VertexHandle]): FaceHandle =
    var vh: MeshType.VertexRef
    let n: int = vertex_handles.len
    var inner_next, inner_prev, outer_next, outer_prev, boundary_next, boundary_prev, patch_sart, path_end: MeshType.HalfedgeRef

    # Check sufficient working storage available

    if mesh.edgeData.len < n:
      mesh.edgeData.setLen(n);
      mesh.next_cache.setLen(6*n)

    var next_cache_count = 0

    # don't allow degenerated faces
    if n < 3:
      echo "PolyMeshT::add_face: not enough vertices"
      return InvalidFaceHandle;


    var v1, v2 = MeshType.VertexRef(mesh: mesh.addr)
    # test for topological errors
    for i in 0 ..< n:
      let ii = (i+1) mod n
      v1.handle = vertex_handles[i]
      v2.handle = vertex_handles[ii]

      if not is_boundary(v1):
        echo "PolyMeshT::add_face: complex vertex"
        return InvalidFaceHandle

      # Initialise edge attributes
      mesh.edgeData[i].halfedge_handle = find_halfedge(v1, v2).handle
      mesh.edgeData[i].is_new = not mesh.edgeData[i].halfedge_handle.is_valid()
      mesh.edgeData[i].needs_adjust = false

      if not mesh.edgeData[i].is_new and not is_boundary(makeRef(mesh, mesh.edgeData[i].halfedge_handle)):
        echo "PolyMeshT::add_face: complex edge"
        return InvalidFaceHandle

    # re-link patches if necessary
    for i in 0 ..< n:
      let ii = (i+1) mod n

      if not mesh.edgeData[i].is_new and not mesh.edgeData[ii].is_new:
        inner_prev = makeRef(mesh, mesh.edgeData[i].halfedge_handle)
        inner_next = makeRef(mesh, mesh.edgeData[ii].halfedge_handle)

        if inner_prev.goNext.handle != inner_next.handle:
          # here comes the ugly part... we have to relink a whole patch

          # search a free gap
          # free gap will be between boundary_prev and boundary_next
          outer_prev = inner_next.goOpp
          outer_next = inner_prev.goOpp
          boundary_prev = outer_prev;
          while true:
            boundary_prev = boundary_prev.goNext.goOpp
            if is_boundary(boundary_prev):
              break
            boundary_next = boundary_prev.goNext

          # ok ?
          if boundary_prev == inner_prev:
            echo "PolyMeshT::add_face: patch re-linking failed"
            return InvalidFaceHandle


          assert(is_boundary(boundary_prev))
          assert(is_boundary(boundary_next))

          # other halfedges' handles
          let patch_start = inner_prev.goNext
          let patch_end   = inner_next.goPrev

          assert(boundary_prev.handle.is_valid())
          assert(patch_start.handle.is_valid())
          assert(patch_end.handle.is_valid())
          assert(boundary_next.handle.is_valid())
          assert(inner_prev.handle.is_valid())
          assert(inner_next.handle.is_valid())

          # relink
          mesh.next_cache[next_cache_count+0] = (boundary_prev.handle, patch_start.handle)
          mesh.next_cache[next_cache_count+1] = (patch_end.handle, boundary_next.handle)
          mesh.next_cache[next_cache_count+2] = (inner_prev.handle, inner_next.handle)
          next_cache_count += 3


    # create missing edges
    for i in 0 ..< n:
      let ii = (i+1) mod n
      if mesh.edgeData[i].is_new:
        mesh.edgeData[i].halfedge_handle = mesh.new_edge(vertex_handles[i], vertex_handles[ii])

    # create the face

    let fh = mesh.addFace()
    fh.connectivity.halfedge_handle = mesh.edgeData[n-1].halfedge_handle

    # setup halfedges
    for i in 0 ..< n:
      let ii = (i+1) mod n
      vh         = makeRef(mesh, vertex_handles[ii])
      inner_prev = makeRef(mesh, mesh.edgeData[i].halfedge_handle)
      inner_next = makeRef(mesh, mesh.edgeData[ii].halfedge_handle)
      assert(inner_prev.handle.is_valid())
      assert(inner_next.handle.is_valid())

      var id = 0;
      if mesh.edgeData[i].is_new:  id += 1
      if mesh.edgeData[ii].is_new: id += 2

      if id != 0:
        outer_prev = goOpp(inner_next)
        outer_next = goOpp(inner_prev)
        assert(outer_prev.handle.is_valid())
        assert(outer_next.handle.is_valid())

        # set outer links
        case id
        of 1: # prev is new, next is old
          boundary_prev = inner_next.goPrev;
          assert(boundary_prev.handle.is_valid())
          mesh.next_cache[next_cache_count] = (boundary_prev.handle, outer_next.handle)
          inc next_cache_count
          vh.connectivity.out_halfedge_handle = outer_next.handle

        of 2: # next is new, prev is old
          boundary_next = inner_prev.goNext;
          assert(boundary_next.handle.is_valid());
          mesh.next_cache[next_cache_count] = (outer_prev.handle, boundary_next.handle)
          inc next_cache_count
          vh.connectivity.out_halfedge_handle = boundary_next.handle

        of 3: # both are new
          if not vh.connectivity.out_halfedge_handle.is_valid():
            vh.connectivity.out_halfedge_handle = outer_next.handle
            mesh.next_cache[next_cache_count] = (outer_prev.handle, outer_next.handle)
            inc next_cache_count
          else:
            boundary_next = vh.goOutHalfedge
            boundary_prev = boundary_next.goPrev
            assert(boundary_prev.handle.is_valid())
            assert(boundary_next.handle.is_valid())
            mesh.next_cache[next_cache_count+0] = (boundary_prev.handle, outer_next.handle)
            mesh.next_cache[next_cache_count+1] = (outer_prev.handle, boundary_next.handle)
            next_cache_count += 2
        else:
          assert false, $id

        # set inner link
        mesh.next_cache[next_cache_count] = (inner_prev.handle, inner_next.handle)
        next_cache_count += 1

      else:
        mesh.edgeData[ii].needs_adjust = (vh.connectivity.out_halfedge_handle == inner_next.handle)

      # set face handle
      mesh.connectivity(mesh.edgeData[i].halfedge_handle).face_handle = fh.handle

    # process next halfedge cache
    for i in 0 ..< next_cache_count:
      let a = mesh.next_cache[i][0]
      let b = mesh.next_cache[i][1]
      mesh.connectivity(a).next_halfedge_handle = b

    # adjust vertices' halfedge handle
    for i in 0 ..< n:
      if mesh.edgeData[i].needs_adjust:
        adjust_outgoing_halfedge(mesh.makeRef(vertex_handles[i]))

    return fh.handle


#[
#TriConnectivity::FaceHandle
#TriConnectivity::add_face(const VertexHandle* _vertex_handles, size_t _vhs_size)
{
  # need at least 3 vertices
  if (_vhs_size < 3) return InvalidFaceHandle;

  #/ face is triangle -> ok
  if (_vhs_size == 3)
    return PolyConnectivity::add_face(_vertex_handles, _vhs_size);

  #/ face is not a triangle -> triangulate
  else
  {
    #omlog() << "triangulating " << _vhs_size << "_gon\n";

    VertexHandle vhandles[3];
    vhandles[0] = _vertex_handles[0];

    FaceHandle fh;
    unsigned int i(1);
    --_vhs_size;

    while (i < _vhs_size)
    {
      vhandles[1] = _vertex_handles[i];
      vhandles[2] = _vertex_handles[++i];
      fh = PolyConnectivity::add_face(vhandles, 3);
    }

    return fh;
  }
}
]#
