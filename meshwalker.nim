import math

proc angle(cos_angle, sin_angle: float32): float32 =
  #sanity checks - otherwise acos will return nan  
  result = arccos(clamp(cos_angle, -1, 1))
  if sin_angle < 0:
    result *= -1

template meshTypeMethodsTemplate*(MeshType: typedesc) =

  proc connectivity(halfedge: MeshType.HalfedgeRef): Halfedge =
    halfedge.mesh.edges[halfedge.handle.int div 2][halfedge.handle.int and 1]

  proc connectivity(vertes: MeshType.VertexRef): Vertex =
    vertes.mesh.vertices[vertes.handle.int]

  proc connectivity(face: MeshType.FaceRef): Face =
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
        pointA = halfedge.goFromVertex.propPoint
        pointB = halfedge.goToVertex.propPoint
    
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
  
  #proc cross(v1,v2: Vec4f): Vec4f =
  #  vec4f(cross(v1.xyz, v2.xyz), 0)
    
  
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
    
  proc calc_dihedral_angle*(halfedge: MeshType.HalfedgeRef) : MeshType.vertexPropertyType(point).T =
    if halfedge.goEdge.is_boundary:
      return 0
  
  
    let 
      n0 = halfedge.calc_sector_normal_unnormalized
      n1 = halfedge.goOpp.calc_sector_normal_unnormalized
      he = halfedge.calc_edge_vector
      denom = n0.norm * n1.norm
           
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
    let denom = v0.norm * v1.norm
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
    return sector_normal.norm / 2
    
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

