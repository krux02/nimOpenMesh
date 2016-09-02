import openmesh, meshwalker, macros

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

# parameter to control export

macro debugAst(ast: typed): stmt =
  echo ast.repr
  result = ast

macro createMeshType*(name, argStmtList: untyped) : stmt =
  name.expectKind nnkIdent
  argStmtList.expectKind nnkStmtList
  let argTypeSection = argStmtList[0]
  argTypeSection.expectKind nnkTypeSection
  result = newNimNode(nnkStmtList)

  let
    propertyCategoryNames    = ["vertex", "face", "edge", "halfedge"]
    propertyCategoryNamesUC  = ["Vertex", "Face", "Edge", "Halfedge"]
  
  var propertiesSequences: array[4, seq[tuple[name, typ: NimNode]]]
  
  for typeDef in argTypeSection:
    let ident = typeDef[0].ident
    case $ident
    of "VertexData":
      propertiesSequences[0] = mapTypeToSeqOfMembers(typeDef)
    of "FaceData":
      propertiesSequences[1] = mapTypeToSeqOfMembers(typeDef)
    of "EdgeData":
      propertiesSequences[2] = mapTypeToSeqOfMembers(typeDef)
    of "HalfedgeData":
      propertiesSequences[3] = mapTypeToSeqOfMembers(typeDef)
    else:
      error("unexpected " & $ident & " expect one of {VertexData, FaceData, EdgeData, HalfedgeData}")

  var propertiesTypes: array[4, Nimnode]
  for i,propertiesSeq in propertiesSequences:
    propertiesTypes[i] = structOfArrays(newIdentNode($name.ident & propertyCategoryNamesUC[i] & "Properties"), propertiesSeq)
    
  let typeSection = nnkTypeSection.newTree(propertiesTypes[0], propertiesTypes[1], propertiesTypes[2], propertiesTypes[3])

  let
    vertexPropertiesTypeIdent   = propertiesTypes[0][0]
    facePropertiesTypeIdent     = propertiesTypes[1][0]
    edgePropertiesTypeIdent     = propertiesTypes[2][0]
    halfedgePropertiesTypeIdent = propertiesTypes[3][0]
  
  var tmp = quote do:
    type
      `name`* = object
        vertices* : seq[Vertex]
        edges*    : seq[Edge]
        faces*    : seq[Face]
        vertexProperties*: `vertexPropertiesTypeIdent`
        faceProperties*: `facePropertiesTypeIdent`
        halfedgeProperties*: `halfedgePropertiesTypeIdent`
        edgeProperties*: `edgePropertiesTypeIdent`

  typeSection.add tmp[0][0]

  result.add typeSection

  # create walker types
  
  var walkerNames : array[4, string]  
  for i, categoryName in propertyCategoryNamesUC:
    let identStr = $name.ident & "_" & categoryName & "Walker"
    walkerNames[i] = identStr
    let
      identNode = newIdentNode(identStr)
      HandleType = newIdentNode(categoryName & "Handle")
    result.add quote do:
      type
        `identNode`* = object
          mesh*: ptr `name`
          handle*: `HandleType`

  # create property accessors from walkers

  for i, propertiesSeq in propertiesSequences:
    let
      propertiesName = newIdentNode(propertyCategoryNames[i] & "Properties")
      walkerIdent = newIdentNode(walkerNames[i])
      
    for tup in propertiesSeq:
      let
        typ = tup.typ
        name = tup.name
      var nameStr = $name.ident
      # first letter to upper case without dependency
      if 'a' <= nameStr[0] and nameStr[0] <= 'z':
        nameStr[0] = char(nameStr[0].int - 32)
      let accessorIdent = newIdentNode("prop" & nameStr)

      result.add quote do:
        proc `accessorIdent`*(walker: `walkerIdent`): var `typ` =    
          walker.mesh.`propertiesName`.`name`[walker.handle.int]

  result.add newCall(bindSym"walkerMethods", name)
  
  #result = newCall(bindSym"debugAst", result)
  
import vecmath
  
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
