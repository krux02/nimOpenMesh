import openmesh, vecmath, math, macros

proc xyz(v: var Vec4f): var Vec3f =
  return cast[ptr Vec3f](v.addr)[]

proc xyz(v: Vec4f): Vec3f =
  vec3f(v[0], v[1], v[2])

proc xyz(v: var Vec3f): var Vec3f = v

proc xyz(v: Vec3f): Vec3f = v

# point and normal need to have the same type

createMeshType(MyMeshType):
  type
    VertexData = object
      point     : Vec4f
      normal    : Vec4f
      color     : Vec4f
      texCoord1 : Vec2f
      texCoord2 : Vec2f

    FaceData = object
      someValue1 : int32

    HalfedgeData = object
      someValue1 : int32
  
proc is_boundary*(halfedge: MyMeshType_HalfedgeRef): bool =
  return not halfedge.goFace.handle.isValid

proc is_boundary*(edge: MyMeshType_EdgeRef): bool =
  for halfedge in edge.circulateHalfedges:
    if halfedge.is_boundary:
      return true
  return false
 
proc is_boundary*(vertex: MyMeshType_VertexRef): bool =
  ## Checks if the associated halfedge (which would on a boundary be the outside
  ## halfedge), is connected to a face. Which is equivalent, if the vertex is
  ## at the boundary of the mesh, as OpenMesh will make sure, that if there is a
  ## boundary halfedge at the vertex, the halfedge will be the one which is associated
  ## to the vertex.
  
  let heh = vertex.goOutHalfedge
  return heh.handle.is_invalid or heh.goFace.handle.is_invalid
  
proc is_boundary*(face: MyMeshType_FaceRef, check_vertex: bool = false): bool =
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

proc is_not_boundary*(face: MyMeshType_FaceRef, check_vertex: bool = false): bool =
  not is_boundary(face, check_vertex)
proc is_not_boundary*(vertex: MyMeshType_VertexRef): bool =
  not is_boundary(vertex)
proc is_not_boundary*(edge: MyMeshType_EdgeRef): bool =
  not is_boundary(edge)
proc is_not_boundary*(halfedge: MyMeshType_HalfedgeRef): bool =
  not is_boundary(halfedge)

proc calc_face_normal(face: MyMeshType_FaceRef): type(face.goHalfedge.goToVertex.propPoint) =
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

  
proc getNormal(face: MyMeshType_FaceRef) : type(face.goHalfedge.goToVertex.propPoint) =
  ## Either returns the normal property when available, or calculates it, when the property is not available
  when hasFaceProperty(MyMeshType, normal):
    return face.propNormal
  else:
    return face.calc_face_normal

proc is_estimated_feature_edge*(halfedge: MyMeshType_HalfedgeRef, feature_angle: float): bool =
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

proc is_not_estimated_feature_edge*(halfedge: MyMeshType_HalfedgeRef, feature_angle: float): bool =
  not is_estimated_feature_edge(halfedge, feature_angle)

#proc cross(v1,v2: Vec4f): Vec4f =
#  vec4f(cross(v1.xyz, v2.xyz), 0)

proc angle(cos_angle, sin_angle: float32): float32 =
  #sanity checks - otherwise acos will return nan  
  result = arccos(clamp(cos_angle, -1, 1))
  if sin_angle < 0:
    result *= -1
  

proc calc_edge_vector*(halfedge: MyMeshType_HalfedgeRef): auto =
  ## Calculates the edge vector as the difference of the
  ## the points defined by to_vertex_handle and from_vertex_handle
  halfedge.goToVertex.propPoint - halfedge.goFromVertex.propPoint

proc calc_edge_vector*(edge: MyMeshType_EdgeRef): auto =
  ## Calculates the edge vector as the difference of the
  ## the points defined by to_vertex_handle and from_vertex_handle
  edge.goHalfedge.calc_edge_vector
    
proc calc_edge_sqr_length*(halfedge: MyMeshType_HalfedgeRef): auto =
  let v = halfedge.calc_edge_vector
  return dot(v,v)

proc calc_edge_sqr_length*(edge: MyMeshType_EdgeRef): auto =
  edge.goHalfedge.calc_edge_sqr_length
  
proc calc_edge_length*(halfedge: MyMeshType_HalfedgeRef): auto =
  halfedge.calc_edge_sqr_length.sqrt
  
proc calc_edge_length*(edge: MyMeshType_EdgeRef): auto =
  edge.goHalfedge.calc_edge_length

proc calc_sector_vectors*(halfedge: MyMeshType_HalfedgeRef) : tuple[vec0, vec1: type(halfedge.goToVertex.propPoint)] =
  ## defines a consistent representation of a sector geometry:
  ## the halfedge _in_heh defines the sector orientation
  ## the vertex pointed by _in_heh defines the sector center
  ## vec0 and vec1 are resp. the first and the second vectors defining the sector
  result.vec0 = halfedge.goNext.calc_edge_vector # p2 - p1
  result.vec1 = halfedge.goOpp.calc_edge_vector  # p0 - p1

proc calc_sector_normal_unnormalized*(halfedge: MyMeshType_HalfedgeRef) : type(halfedge.goToVertex.propPoint) =
  ## calculates the normal (non-normalized) of the face sector defined by
  ## the angle <(_in_heh,next_halfedge(_in_heh))
  let (vec0, vec1) = halfedge.calc_sector_vectors
  xyz(result) = cross(vec0.xyz, vec1.xyz) # (p2-p1)^(p0-p1)
  
proc calc_dihedral_angle*(halfedge: MyMeshType_HalfedgeRef) : type(halfedge.goToVertex.propPoint[0]) =
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

proc calc_dihedral_angle_fast*(halfedge: MyMeshType_HalfedgeRef): type(halfedge.goToVertex.propPoint[0]) =
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
  
proc calc_face_centroid*(face: MyMeshType_FaceRef): type(face.goHalfedge.goToVertex.propPoint) =
  ## calculates the average of the vertices defining the face
  var valence: type(result[0])
  for vertex in face.circulateVertices:
    result += vertex.propPoint
    valence += 1
  
  result = result / valence
  
proc calc_halfedge_normal*(inHalfedge: MyMeshType_HalfedgeRef, feature_angle: float64): type(inHalfedge.goToVertex.propPoint) =
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
    

proc calc_sector_angle*(halfedge: MyMeshType_HalfedgeRef): type(halfedge.goToVertex.propPoint[0]) =
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


proc calc_sector_area*(halfedge: MyMeshType_HalfedgeRef): type(halfedge.goToVertex.propPoint[0]) =
  ## calculates the area of the face sector defined by
  ## the angle ``(inHalfedge,inHalfedge.goNext)``
  ## NOTE: special cases (e.g. concave sectors) are not handled correctly
  let sector_normal = halfedge.calc_sector_normal_unnormalized
  return sector_normal.norm / 2
  
proc calc_vertex_normal_fast*(vertex: MyMeshType_VertexRef): type(vertex.propPoint) =
  for face in vertex.circulateFaces:
    result = result + face.getNormal

proc calc_vertex_normal*(vertex: MyMeshType_VertexRef): type(vertex.propPoint) =
  result = vertex.calc_vertex_normal_fast
  let length_sq = dot(result, result) # squared length
  if length_sq != 0:
    result = result / sqrt(length_sq)

import loopschememask
    
proc calc_vertex_normal_loop*(vertex: MyMeshType_VertexRef): type(vertex.propPoint) =

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

proc calc_vertex_normal_correct*(vertex: MyMeshType_VertexRef): type(vertex.propPoint) =
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

    
