import openmesh, vecmath, math

createMeshType(MyMeshType):
  type
    VertexData = object
      point    : Vec4f
      normal   : Vec4f
      color    : Vec4f
      texCoord : Vec2f

    FaceData = object
      normal   : Vec3f
      color    : Vec3f

    HalfedgeData = object
      someValue : int32

template has_edge_status(mesh: MyMeshType): bool =
  false
      
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

proc is_estimated_feature_edge(walker: MyMeshType_HalfedgeWalker, feature_angle: float): bool =
  let edge = walker.goEdge

  when walker.mesh[].has_edge_status:
    if edge.status.feature:
      return true;
  
  if edge.is_boundary:
    return false;

  # compute angle between faces
  let
    faceNormal0 = walker.goFace.propNormal
    faceNormal1 = walker.goOpp.goFace.propNormal

  return dot(faceNormal0,faceNormal1) < cos(feature_angle)

proc sqrnorm(v : Vec4f) : auto =
  dot(v,v)

proc `xyz=`(v: var Vec4f, val: Vec3f): void =
  v[0] = val[0]
  v[1] = val[1]
  v[2] = val[2]

proc `yzx`(v : Vec4f): Vec3f =
  result[0] = v[1]
  result[1] = v[2]
  result[2] = v[0]

proc `zxy`(v : Vec4f): Vec3f =
  result[0] = v[2]
  result[1] = v[0]
  result[2] = v[1]
  
proc cross(v1,v2: Vec4f): Vec4f =
  vec4f(cross(v1.xyz, v2.xyz), 0)

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
  walker.calc_edge_vector.sqrnorm

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

  
proc calc_sector_normal(walker: MyMeshType_HalfedgeWalker) : type(walker.goToVertex.propPoint) =
  let (vec0, vec1) = walker.calc_sector_vectors
  result.xyz = cross(vec0.xyz, vec1.xyz); # (p2-p1)^(p0-p1)
  
proc calc_dihedral_angle(walker: MyMeshType_HalfedgeWalker) : type(walker.goToVertex.propPoint[0]) =
  if walker.goEdge.is_boundary:
    return 0

  let 
    n0 = walker.calc_sector_normal
    n1 = walker.goOpp.calc_sector_normal
    he = walker.calc_edge_vector
    denom = n0.norm * n1.norm
         
  if denom == 0:
    return 0

  var
    da_cos = dot(n0, n1) / denom
    da_sin_sign = dot(cross(n0, n1), he) #should be normalized, but we need only the sign

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
    da_sin_sign = dot(cross(n0, n1), he.xyz) #should be normalized, but we need only the sign
  
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

proc calc_halfedge_normal(walker: MyMeshType_HalfedgeWalker, feature_angle: float64): type(walker.goToVertex.propPoint) =

  ## Calculate halfedge normal for one specific halfedge

  ## Calculate normal vector for halfedge _heh.
   
  ## Face normals have to be computed first!

  # TODO solution for Attribute normals and face normals to be ensured
  ## Needs the Attributes::Normal attribute for faces and vertices.
  ## Call ``request_face_normals()`` and ``request_halfedge_normals()`` before using it! (if it wolud be openmesh in c++ ;)
  ## If the dihedral angle across this edge is greater than ``feature_angle``, the edge is considered as a feature edge (angle in radians).
  if walker.is_boundary:
    return
    
  else:
  
    var
      fhs = newSeq[FaceHandle](0)
      heRef = walker
      

    # collect CW face-handles
    block:
      fhs.add heRef.goFace.handle
      heRef = heRef.goNext.goOpp
      while heRef.handle != walker.handle and not heRef.is_boundary and not heRef.is_estimated_feature_edge(feature_angle):

    # collect CCW face-handles
    if (heh != _heh && !is_estimated_feature_edge(_heh, _feature_angle))
    {
      heh = Kernel::opposite_halfedge_handle(_heh);

      if ( !Kernel::is_boundary(heh) ) {
        do
        {

          fhs.push_back(Kernel::face_handle(heh));

          heh = Kernel::prev_halfedge_handle(heh);
          heh = Kernel::opposite_halfedge_handle(heh);
        }
        while(!Kernel::is_boundary(heh) && !is_estimated_feature_edge(heh, _feature_angle));
      }
    }

    Normal n(0,0,0);
    for(unsigned int i=0; i<fhs.size(); ++i)
      n += Kernel::normal(fhs[i]);

    return n.normalize();
  }
}

    
proc calc_halfedge_normal
proc calc_sector_angle
proc calc_sector_angle_cos_sin
proc calc_sector_area

proc calc_sector_vectors
proc calc_vertex_normal
proc calc_vertex_normal_correct
proc calc_vertex_normal_fast
proc calc_vertex_normal_loop
