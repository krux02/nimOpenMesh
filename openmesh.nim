#this is supposed to be a nim port of open mesh.
import macros, meshwalker

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
    vertices* : seq[Vertex]
    edges*    : seq[Edge]
    faces*    : seq[Face]
    
template handleProcs(HandleType: typedesc) =
  proc `==`*(handleA, handleB: HandleType): bool =
    handleA == handleB

  proc is_valid*(handle: HandleType): bool =
    handle.int >= 0

  proc is_invalid*(handle: HandleType): bool =
    handle.int < 0
    
handleProcs(VertexHandle)
handleProcs(HalfedgeHandle)
handleProcs(EdgeHandle)
handleProcs(FaceHandle)

macro debugAst*(ast: typed): untyped =
  echo ast.repr
  result = ast
  
macro hasProperty(tpe: typed; propertyType, ident: untyped): bool =
  block b:
    for identDefs in tpe.symbol.getImpl[2][2]:
      if identDefs[0][1].ident == propertyType.ident:
        for innerIdentDefs in identDefs[1].symbol.getImpl[2][2]:
          if innerIdentDefs[0].ident == ident.ident:
            result = newLit(true)
            break b
    result = newLit(false)

macro propertyType(tpe: typed; propertyType, ident: untyped): untyped =
  block b:
    for identDefs in tpe.symbol.getImpl[2][2]:
      if identDefs[0][1].ident == propertyType.ident:
        for innerIdentDefs in identDefs[1].symbol.getImpl[2][2]:
          if innerIdentDefs[0].ident == ident.ident:
            return innerIdentDefs[1][1]
            
template vertexPropertyType*(tpe: typedesc; ident: untyped): typedesc =
  propertyType(MyMeshType, vertexProperties, point)

template facePropertyType*(tpe: typedesc; ident: untyped): typedesc =
  propertyType(MyMeshType, faceProperties, point)

template edgePropertyType*(tpe: typedesc; ident: untyped): typedesc =
  propertyType(MyMeshType, edgeProperties, point)

template halfedgePropertyType*(tpe: typedesc; ident: untyped): typedesc =
  propertyType(MyMeshType, halfedgeProperties, point)
  
template hasVertexProperty*(tpe: typedesc, ident: untyped): bool =
  hasProperty(tpe, vertexProperties, ident)

template hasFaceProperty*(tpe: typedesc, ident: untyped): bool =
  hasProperty(tpe, faceProperties, ident)

template hasHalfedgeProperty*(tpe: typedesc, ident: untyped): bool =
  hasProperty(tpe, halfedgeProperties, ident)

template hasEdgeProperty*(tpe: typedesc, ident: untyped): bool =
  hasProperty(tpe, edgeProperties, ident)

proc structOfArrays(name: NimNode, members: seq[tuple[name, typ: NimNode]]) : NimNode =

  var mappedMembers = newSeq[tuple[name,typ:NimNode]](len(members))

  for i,tup in members:
    mappedMembers[i].name = tup.name
    mappedMembers[i].typ  = nnkBracketExpr.newTree(ident("seq"), tup.typ)

  result = quote do:
    type `name` = object
 
  let recList = newNimNode(nnkRecList)
  #result = result[0][0] # peel StmtList and TypeSection
  #result[2][2] = recList
  result[0][0][2][2] = recList
  
  for tup in mappedMembers:
    recList.add nnkIdentDefs.newTree(tup.name, tup.typ, newEmptyNode())

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

macro createMeshType*(name, argStmtList: untyped): auto =

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
    let node = structOfArrays(ident($name.ident & propertyCategoryNamesUC[i] & "Properties"), propertiesSeq)
    result.add node
    propertiesTypes[i] = node[0][0]
    

  let
    vertexPropertiesTypeIdent   = propertiesTypes[0][0]
    facePropertiesTypeIdent     = propertiesTypes[1][0]
    edgePropertiesTypeIdent     = propertiesTypes[2][0]
    halfedgePropertiesTypeIdent = propertiesTypes[3][0]

  result.add quote do:
    type
      `name`* = object
        vertices* : seq[Vertex]
        edges*    : seq[Edge]
        faces*    : seq[Face]
        vertexProperties*: `vertexPropertiesTypeIdent`
        faceProperties*: `facePropertiesTypeIdent`
        halfedgeProperties*: `halfedgePropertiesTypeIdent`
        edgeProperties*: `edgePropertiesTypeIdent`

  # create walker types
  
  var typeNames : array[4, string]  
  for i, categoryName in propertyCategoryNamesUC:
    let identStr = $name.ident & "_" & categoryName & "Ref"
    
    let typeAccessorName = newIdentNode(categoryName & "Ref")
    
    typeNames[i] = identStr
    let
      identNode = ident(identStr)
      HandleType = ident(categoryName & "Handle")
    result.add quote do:
      type
        `identNode`* = object
          mesh*: ptr `name`
          handle*: `HandleType`

      template `typeAccessorName`*(tpe: typedesc[`name`]): typedesc =
        `identNode`
        

  # create property accessors from walkers

  for i, propertiesSeq in propertiesSequences:
    let
      propertiesName = ident(propertyCategoryNames[i] & "Properties")
      refIdent = ident(typeNames[i])
      
    for tup in propertiesSeq:
      let
        typ = tup.typ
        name = tup.name
      var nameStr = $name.ident
      # first letter to upper case without dependency
      if 'a' <= nameStr[0] and nameStr[0] <= 'z':
        nameStr[0] = char(nameStr[0].int - 32)
      let accessorIdent = ident("prop" & nameStr)

      result.add quote do:
        proc `accessorIdent`*(walker: `refIdent`): var `typ` =    
          walker.mesh.`propertiesName`.`name`[walker.handle.int]

  result.add quote do:
    meshTypeMethodsTemplate(`name`)
    

  #echo result.repr
  #result = newCall(bindSym"debugAst", result)




