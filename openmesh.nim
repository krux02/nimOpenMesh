#this is supposed to be a nim port of open mesh.

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

macro debugAst*(ast: typed): stmt =
  echo ast.repr
  result = ast

include meshwalker, property

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
  
  var walkerNames : array[4, string]  
  for i, categoryName in propertyCategoryNamesUC:
    let identStr = $name.ident & "_" & categoryName & "Walker"
    walkerNames[i] = identStr
    let
      identNode = ident(identStr)
      HandleType = ident(categoryName & "Handle")
    result.add quote do:
      type
        `identNode`* = object
          mesh*: ptr `name`
          handle*: `HandleType`

  # create property accessors from walkers

  for i, propertiesSeq in propertiesSequences:
    let
      propertiesName = ident(propertyCategoryNames[i] & "Properties")
      walkerIdent = ident(walkerNames[i])
      
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
        proc `accessorIdent`*(walker: `walkerIdent`): var `typ` =    
          walker.mesh.`propertiesName`.`name`[walker.handle.int]

  result.add newCall(bindSym"walkerMethods", name)

  #echo result.repr
  #result = newCall(bindSym"debugAst", result)
