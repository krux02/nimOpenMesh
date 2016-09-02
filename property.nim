import openmesh, macros

import vecmath

proc newObjectTypeDef(name: NimNode, recSeq: seq[tuple[name, typ: NimNode]]): NimNode =
  var recList : NimNode
  if recSeq.len == 0:
    recList = newEmptyNode()
  else:
    recList = newNimNode(nnkRecList)

  for tup in recSeq:
    recList.add nnkIdentDefs.newTree(tup.name, tup.typ, newEmptyNode())
    
  result = nnkTypeDef.newTree(
    name,
    newEmptyNode(),
    nnkObjectTy.newTree(
        newEmptyNode(),
        newEmptyNode(),
        recList
    )
  )
  
proc structOfArrays(name: NimNode, members: seq[tuple[name, typ: NimNode]]) : NimNode =

  var mappedMembers = newSeq[tuple[name,typ:NimNode]](len(members))

  for i,tup in members:
    mappedMembers[i].name = tup.name
    mappedMembers[i].typ  = nnkBracketExpr.newTree(newIdentNode("seq"), tup.typ)
    
  result = newObjectTypeDef(name, mappedMembers)

proc mapTypeToSeqOfMembers(typeDef: NimNode): seq[tuple[name, typ: NimNode]] =
  typeDef.expectKind nnkTypeDef

  typeDef[1].expectKind nnkEmpty
  typeDef[2].expectKind nnkObjectTy
  typeDef[2][0].expectKind nnkEmpty
  typeDef[2][1].expectKind nnkEmpty
  typeDef[2][2].expectKind nnkRecList

  result.newSeq(0)

  for identDefs in typeDef[2][2]:
    identDefs.expectKind nnkIdentDefs
    identDefs[0].expectKind nnkIdent
    identDefs[1].expectKind nnkIdent
    identDefs[2].expectKind nnkEmpty

    result.add( (name: identDefs[0], typ: identDefs[1]) )

macro createMeshType(name, argStmtList: untyped) : stmt =
  name.expectKind nnkIdent
  argStmtList.expectKind nnkStmtList
  let argTypeSection = argStmtList[0]
  argTypeSection.expectKind nnkTypeSection
  result = newNimNode(nnkStmtList)

  
  
  var vertexPropertiesSeq, facePropertiesSeq, edgePropertiesSeq, halfedgePropertiesSeq: seq[tuple[name, typ: NimNode]]

  for typeDef in argTypeSection:
    let ident = typeDef[0].ident
    case $ident
    of "VertexData":
      vertexPropertiesSeq = mapTypeToSeqOfMembers(typeDef)
    of "FaceData":
      facePropertiesSeq = mapTypeToSeqOfMembers(typeDef)
    of "EdgeData":
      edgePropertiesSeq = mapTypeToSeqOfMembers(typeDef)
    of "HalfedgeData":
      halfedgePropertiesSeq = mapTypeToSeqOfMembers(typeDef)
    else:
      error("unexpected identifier " & $ident)

  let
    vertexPropertiesType = structOfArrays(newIdentNode($name.ident & "VertexProperties"), vertexPropertiesSeq)
    facePropertiesType = structOfArrays(newIdentNode($name.ident & "FaceProperties"), facePropertiesSeq)
    edgePropertiesType = structOfArrays(newIdentNode($name.ident & "EdgeProperties"), edgePropertiesSeq)
    halfedgePropertiesType = structOfArrays(newIdentNode($name.ident & "HalfedgeProperties"), halfedgePropertiesSeq)

  let typeSection = nnkTypeSection.newTree(vertexPropertiesType, facePropertiesType, edgePropertiesType, halfedgePropertiesType)

  let
    vertexPropertiesTypeIdent   = vertexPropertiesType[0]
    facePropertiesTypeIdent     = facePropertiesType[0]
    edgePropertiesTypeIdent     = edgePropertiesType[0]
    halfedgePropertiesTypeIdent = halfedgePropertiesType[0]
  
  var tmp = quote do:
    type
      `name` = object
        vertexProperties: `vertexPropertiesTypeIdent`
        faceProperties: `facePropertiesTypeIdent`
        halfedgeProperties: `halfedgePropertiesTypeIdent`
        edgeProperties: `edgePropertiesTypeIdent`

  typeSection.add tmp[0][0]

  let
    vertexWalkerIdent   = newIdentNode("Vertex"   & $name.ident & "Walker")
    faceWalkerIdent     = newIdentNode("Face"     & $name.ident & "Walker")
    edgeWalkerIdent     = newIdentNode("Edge"     & $name.ident & "Walker")
    halfedgeWalkerIdent = newIdentNode("Halfedge" & $name.ident & "Walker")

  result.add typeSection
    
  result.add quote do:
      type
        `vertexWalkerIdent` = object
          mesh: ptr `name`
          handle: VertexHandle

        `faceWalkerIdent` = object
          mesh: ptr `name`
          handle: FaceHandle

        `edgeWalkerIdent` = object
          mesh: ptr `name`
          handle: EdgeHandle

        `halfedgeWalkerIdent` = object
          mesh: ptr `name`
          handle: HalfedgeHandle


  
  for tup in vertexPropertiesSeq:
    let
      typ = tup.typ
      name = tup.name
    result.add quote do:
      proc `name`(walker: `vertexWalkerIdent`): var `typ` =    
        walker.mesh.vertexProperties.`name`[walker.handle.int]

  for tup in facePropertiesSeq:
    let
      typ = tup.typ
      name = tup.name
    result.add quote do:
      proc `name`(walker: `faceWalkerIdent`): var `typ` =
        walker.mesh.faceProperties.`name`[walker.handle.int]

  for tup in edgePropertiesSeq:
    let
      typ = tup.typ
      name = tup.name
    result.add quote do:
      proc `name`(walker: `edgeWalkerIdent`): var `typ` =
        walker.mesh.edgeProperties.`name`[walker.handle.int]
    
  for tup in halfedgePropertiesSeq:
    let
      typ = tup.typ
      name = tup.name
    result.add quote do:
      proc `name`(walker: `halfedgeWalkerIdent`): var `typ` =
        walker.mesh.halfedgeProperties.`name`[walker.handle.int]


  echo result.repr
        
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

# this should be generated

type
  VertexProperties = object
    pointData    : seq[Vec4f]
    normalData   : seq[Vec4f]
    colorData    : seq[Vec4f]
    texCoordData : seq[Vec2f]

  FaceProperties = object
    normalData : seq[Vec3f]
    colorData  : seq[Vec3f]

  HalfedgeProperties = object
    someValueData : seq[int32]

  EdgeProperties = object

  DataMesh = object
    vertexProperties: VertexProperties
    faceProperties: FaceProperties
    halfedgeProperties: HalfedgeProperties
    edgeProperties: Edgeproperties
    
  HalfedgeDataWalker* = object
    mesh: ptr DataMesh
    handle: HalfedgeHandle

  VertexDataWalker* = object
    mesh: ptr DataMesh
    handle: VertexHandle

  FaceDataWalker* = object
    mesh: ptr DataMesh
    handle: FaceHandle

  EdgeDataWalker* = object
    mesh: ptr DataMesh
    handle: EdgeHandle

proc someValue(walker: HalfedgeDataWalker): var int32 =
  walker.mesh.halfedgeProperties.someValueData[walker.handle.int]

proc point*(walker: VertexDataWalker): var Vec4f =
  walker.mesh.vertexProperties.pointData[walker.handle.int]

proc normal*(walker: VertexDataWalker): var Vec4f =
  walker.mesh.vertexProperties.normalData[walker.handle.int]

proc color(walker: VertexDataWalker): var Vec4f =
  walker.mesh.vertexProperties.colorData[walker.handle.int]

proc texCoord(walker: VertexDataWalker): var Vec2f =
  walker.mesh.vertexProperties.texCoordData[walker.handle.int]

proc normal(walker: FaceDataWalker): var Vec3f =
  walker.mesh.faceProperties.normalData[walker.handle.int]

proc color(walker: FaceDataWalker): var Vec3f =
  walker.mesh.faceProperties.colorData[walker.handle.int]


