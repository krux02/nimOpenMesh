import openmesh, vecmath, math

# point and normal need to have the same type

createMeshType(MyMeshType):
  type
    VertexData = object
      point    : Vec4f
      normal   : Vec4f
      color    : Vec4f
      texCoord : Vec2f

    FaceData = object
      normal   : Vec4f
      color    : Vec3f

    HalfedgeData = object
      someValue : int32

template has_edge_status*(mesh: typedesc[MyMeshType]): bool =
  false

template has_face_normals*(mesh: typedesc[MyMeshType]): bool =
  true
  
proc is_boundary*(walker: MyMeshType_HalfedgeWalker): bool =
  return not walker.goFace.handle.isValid

proc is_boundary*(walker: MyMeshType_EdgeWalker): bool =
  for halfedge in walker.circulateHalfedges:
    if halfedge.is_boundary:
      return true
  return false
 
proc is_boundary*(walker: MyMeshType_VertexWalker): bool =
  ## Checks if the associated halfedge (which would on a boundary be the outside
  ## halfedge), is connected to a face. Which is equivalent, if the vertex is
  ## at the boundary of the mesh, as OpenMesh will make sure, that if there is a
  ## boundary halfedge at the vertex, the halfedge will be the one which is associated
  ## to the vertex.
  
  let heh = walker.goOutHalfedge

  return heh.handle.is_invalid or heh.goFace.handle.is_invalid


  
proc is_boundary*(faceRef: MyMeshType_FaceWalker, check_vertex: bool = false): bool =
  ## Is face ``faceRef`` at boundary, i.e. is one of its edges (or vertices)
  ## a boundary edge?
  ##
  ## If ``check_vertex`` is true, the corner vertices of the face are checked, too

  # TODO this is ported like this from open mesh, but wouldn't it be faster when only out halfedges are checked?
  for edge in faceRef.circulateEdges: # faceRef.circulateOutHalfedges
    if edge.is_boundary:
      return true

  if check_vertex:
    for vertex in faceRef.circulateVertices:
      if vertex.is_boundary:
        return true
         
  return false

proc is_not_boundary*(faceRef: MyMeshType_FaceWalker, check_vertex: bool = false): bool =
  not is_boundary(faceRef, check_vertex)
proc is_not_boundary*(walker: MyMeshType_VertexWalker): bool =
  not is_boundary(walker)
proc is_not_boundary*(walker: MyMeshType_EdgeWalker): bool =
  not is_boundary(walker)
proc is_not_boundary*(walker: MyMeshType_HalfedgeWalker): bool =
  not is_boundary(walker)

proc is_estimated_feature_edge*(walker: MyMeshType_HalfedgeWalker, feature_angle: float): bool =
  let edge = walker.goEdge

  when walker.mesh[].type.has_edge_status:
    if edge.status.feature:
      return true;
  
  if edge.is_boundary:
    return false;

  # compute angle between faces
  let
    faceNormal0 = walker.goFace.propNormal
    faceNormal1 = walker.goOpp.goFace.propNormal

  return dot(faceNormal0,faceNormal1) < cos(feature_angle)

proc is_not_estimated_feature_edge*(walker: MyMeshType_HalfedgeWalker, feature_angle: float): bool =
  not is_estimated_feature_edge(walker, feature_angle)

proc xyz(v: var Vec4f): var Vec3f =
  return cast[ptr Vec3f](v.addr)[]

proc xyz(v: Vec4f): Vec3f =
  vec3f(v[0], v[1], v[2])

proc xyz(v: var Vec3f): var Vec3f = v

proc xyz(v: Vec3f): Vec3f = v

#proc cross(v1,v2: Vec4f): Vec4f =
#  vec4f(cross(v1.xyz, v2.xyz), 0)

proc angle(cos_angle, sin_angle: float32): float32 =
  #sanity checks - otherwise acos will return nan  
  result = arccos(clamp(cos_angle, -1, 1))
  if sin_angle < 0:
    result *= -1
  

proc calc_edge_vector(halfedgeRef: MyMeshType_HalfedgeWalker): auto =
  ## Calculates the edge vector as the difference of the
  ## the points defined by to_vertex_handle and from_vertex_handle
  halfedgeRef.goToVertex.propPoint - halfedgeRef.goFromVertex.propPoint

proc calc_edge_vector(walker: MyMeshType_EdgeWalker): auto =
  ## Calculates the edge vector as the difference of the
  ## the points defined by to_vertex_handle and from_vertex_handle
  walker.goHalfedge.calc_edge_vector
    
proc calc_edge_sqr_length(walker: MyMeshType_HalfedgeWalker): auto =
  let v = walker.calc_edge_vector
  return dot(v,v)

proc calc_edge_sqr_length(walker: MyMeshType_EdgeWalker): auto =
  walker.goHalfedge.calc_edge_sqr_length
  
proc calc_edge_length(walker: MyMeshType_HalfedgeWalker): auto =
  walker.calc_edge_sqr_length.sqrt
  
proc calc_edge_length(walker: MyMeshType_EdgeWalker): auto =
  walker.goHalfedge.calc_edge_length

proc calc_sector_vectors(walker: MyMeshType_HalfedgeWalker) : tuple[vec0, vec1: type(walker.goToVertex.propPoint)] =
  ## defines a consistent representation of a sector geometry:
  ## the halfedge _in_heh defines the sector orientation
  ## the vertex pointed by _in_heh defines the sector center
  ## vec0 and vec1 are resp. the first and the second vectors defining the sector
  result.vec0 = walker.goNext.calc_edge_vector # p2 - p1
  result.vec1 = walker.goOpp.calc_edge_vector  # p0 - p1

proc calc_sector_normal_unnormalized*(walker: MyMeshType_HalfedgeWalker) : type(walker.goToVertex.propPoint) =
  ## calculates the normal (non-normalized) of the face sector defined by
  ## the angle <(_in_heh,next_halfedge(_in_heh))
  let (vec0, vec1) = walker.calc_sector_vectors
  xyz(result) = cross(vec0.xyz, vec1.xyz) # (p2-p1)^(p0-p1)
  
proc calc_dihedral_angle(walker: MyMeshType_HalfedgeWalker) : type(walker.goToVertex.propPoint[0]) =
  if walker.goEdge.is_boundary:
    return 0

  let 
    n0 = walker.calc_sector_normal_unnormalized
    n1 = walker.goOpp.calc_sector_normal_unnormalized
    he = walker.calc_edge_vector
    denom = n0.norm * n1.norm
         
  if denom == 0:
    return 0

  var
    da_cos = dot(n0, n1) / denom
    da_sin_sign = dot(cross(n0.xyz, n1.xyz), he.xyz) #should be normalized, but we need only the sign

  return angle(da_cos, da_sin_sign)

proc calc_dihedral_angle_fast(walker: MyMeshType_HalfedgeWalker): type(walker.goFace.propNormal[0]) =
  ## Calculates the dihedral angle on the halfedge.
  ## Attention Needs the Attributes::Normal attribute for faces.
  if walker.goEdge.is_boundary:
    return 0

  let
    n0 = walker.goFace.propNormal
    n1 = walker.goOpp.goFace.propNormal
    
    he = walker.calc_edge_vector
    da_cos = dot(n0, n1)
    da_sin_sign = dot(cross(n0.xyz, n1.xyz), he.xyz) #should be normalized, but we need only the sign
  
  return angle(da_cos, da_sin_sign)
  
proc calc_face_centroid(faceRef: MyMeshType_FaceWalker): type(faceRef.goHalfedge.goToVertex.propPoint) =
  ## calculates the average of the vertices defining the face
  var valence: type(result[0])
  for vertex in faceRef.circulateVertices:
    result += vertex.propPoint
    valence += 1
  
  result = result / valence
  
proc calc_face_normal(walker: MyMeshType_FaceWalker): type(walker.goHalfedge.goToVertex.propPoint) =
  assert(walker.goHalfedge.handle.isValid)
  # http://www.opengl.org/wiki/Calculating_a_Surface_Normal
  
  for halfedge in walker.circulateInHalfedges:
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

proc calc_halfedge_normal*(inHalfedge: MyMeshType_HalfedgeWalker, feature_angle: float64): type(inHalfedge.goFace.propNormal) =
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
    result += halfedge.goFace.propNormal
    halfedge = halfedge.goNext.goOpp
    
    if halfedge.handle == inHalfedge.handle or halfedge.is_boundary or halfedge.is_estimated_feature_edge(feature_angle):
      break
  
  # collect CCW face-handles
  if halfedge.handle != inHalfedge.handle and halfedge.is_not_estimated_feature_edge(feature_angle):
    halfedge = inHalfedge.goOpp
    
    if halfedge.is_not_boundary:
      while true:
        result += halfedge.goFace.propNormal
        halfedge = halfedge.goPrev.goOpp
      
        if halfedge.is_boundary or halfedge.is_estimated_feature_edge(feature_angle):
          break
      
    result = result.normalize
    

proc calc_sector_angle*(inHalfedge: MyMeshType_HalfedgeWalker): type(inHalfedge.goToVertex.propPoint[0]) =
  ## The vertex pointed by ``inHalfedge`` defines the sector center
  ## The angle will be calculated between the given halfedge and the next halfedge.
  ## Seen from the center vertex this will be the next halfedge in clockwise direction.
  ## NOTE: only boundary concave sectors are treated correctly, result can be zero


  let (v0,v1) = inHalfedge.calc_sector_vectors()
  let denom = v0.norm * v1.norm
  if denom == 0:
    return 0;
  let cos_a = dot(v0, v1) / denom

  if inHalfedge.is_boundary:
    # determine if the boundary sector is concave or convex
    let
      face = inHalfedge.goOpp.goFace
      fn = face.calc_face_normal # this normal is (for convex fh) OK
      sign_a = dot(cross(v0.xyz, v1.xyz), fn.xyz)

    return angle(cos_a, sign_a)

  else:
    return arccos(clamp(cos_a,-1,1));


proc calc_sector_area*(inHalfedge: MyMeshType_HalfedgeWalker): type(inHalfedge.goToVertex.propPoint[0]) =
  ## calculates the area of the face sector defined by
  ## the angle ``(inHalfedge,inHalfedge.goNext)``
  ## NOTE: special cases (e.g. concave sectors) are not handled correctly
  let sector_normal = inHalfedge.calc_sector_normal_unnormalized
  return sector_normal.norm / 2
  
proc calc_vertex_normal_fast*(inVertex: MyMeshType_VertexWalker): type(inVertex.goOutHalfedge.goFace.propNormal) =
  for face in inVertex.circulateFaces:
    result = result + face.propNormal

proc calc_vertex_normal*(inVertex: MyMeshType_VertexWalker): type(inVertex.goOutHalfedge.goFace.propNormal) =
  result = inVertex.calc_vertex_normal_fast
  let length_sq = dot(result, result) # squared length
  if length_sq != 0:
    result = result / sqrt(length_sq)

import loopschememask
    
proc calc_vertex_normal_loop*(inVertex: MyMeshType_VertexWalker): type(inVertex.goOutHalfedge.goFace.propNormal) =

  #static const LoopSchemeMaskDouble& loop_scheme_mask__ =
  #                LoopSchemeMaskDoubleSingleton::Instance();

  let valence = inVertex.valence

  var
    t_w: type(inVertex.propPoint)
    t_v: type(inVertex.propNormal)
  
  var i = 0
  for vertex in inVertex.circulateVertices:
    t_v = t_v + loopschememask.tang0_weight(valence, i) * vertex.propNormal
    t_w = t_w + loopschememask.tang1_weight(valence, i) * vertex.propPoint
    i += 1
    
  # hack: should be cross(t_v, t_w), but then the normals are reversed?
  result.xyz() = cross(t_w.xyz, t_v.xyz)

proc calc_vertex_normal_correct*(inVertex: MyMeshType_VertexWalker): type(inVertex.propNormal) =
  ## there is no correct way to calculate the vertex normal
  if inVertex.goOutHalfedge.handle.is_invalid:
    # dont crash on isolated vertices
    return

  var in_he_vec = inVertex.goOutHalfedge.goOpp.calc_edge_vector

  for inHalfedge in inVertex.circulateInHalfedges:
    # calculates the sector normal defined by ``halfedge`` and adds it to ``result``
    if inHalfedge.is_boundary:
      continue
    
    let outHalfedge = inHalfedge.goNext
    let out_he_vec = outHalfedge.calc_edge_vector

    result.xyz() += cross(in_he_vec.xyz, out_he_vec.xyz); # sector area is taken into account

    in_he_vec = - out_he_vec; # change the orientation
