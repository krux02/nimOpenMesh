import openmesh, glm

createMeshType(MyMeshType):
  debug
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
