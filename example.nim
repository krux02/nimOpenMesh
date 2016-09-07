import openmesh, vecmath

proc xyz(v: var Vec4f): var Vec3f =
  return cast[ptr Vec3[float32]](v.addr)[]

proc xyz(v: Vec4f): Vec3f =
  result[0] = v[0]
  result[1] = v[1]
  result[2] = v[2]

#proc xyz(v: var Vec3f): var Vec3f = v

#proc xyz(v: Vec3f): Vec3f = v

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
      someValue2 : int32

    EdgeData = object
      someValue3 : int32
