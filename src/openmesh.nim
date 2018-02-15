#this is supposed to be a nim port of open mesh.
import macros, meshTypeMethodsTemplate, hashes

type
  VertexHandle*      = distinct int
  HalfedgeHandle*    = distinct int
  EdgeHandle*        = distinct int
  FaceHandle*        = distinct int

  HandleType = VertexHandle | HalfedgeHandle | EdgeHandle | FaceHandle

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

proc `==`*(handleA, handleB: HandleType): bool =
  var a : int = handleA.int
  var b : int = handleB.int
  a == b

proc hash*(handle: HandleType): Hash =
  hash(handle.int)

proc is_valid*(handle: HandleType): bool =
  handle.int >= 0

proc is_invalid*(handle: HandleType): bool =
  handle.int < 0

proc `$`*(handle: HandleType): string =
  $int(handle)

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

proc structOfArraysTypeDef(name: NimNode, members: seq[tuple[name, typ: NimNode]]) : NimNode =

  var mappedMembers = newSeq[tuple[name,typ:NimNode]](len(members))

  for i,tup in members:
    mappedMembers[i].name = tup.name
    mappedMembers[i].typ  = nnkBracketExpr.newTree(ident("seq"), tup.typ)


  result = quote do:
    type `name` = object

  result.expectKind nnkTypeSection # this was a breaking change in Nim

  let recList = newNimNode(nnkRecList)
  #result = result[0][0] # peel StmtList and TypeSection
  #result[2][2] = recList

  for tup in mappedMembers:
    recList.add nnkIdentDefs.newTree(tup.name, tup.typ, newEmptyNode())

  result[0][2][2] = recList

  result = nnkTypeDef.newTree(
    name,
    newEmptyNode(),
    nnkObjectTy.newTree(
      newEmptyNode(),
      newEmptyNode(),
      recList
    )
  )

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

macro createMeshType*(name, argStmtList: untyped): untyped =

  name.expectKind nnkIdent
  argStmtList.expectKind nnkStmtList
  let argTypeSection = argStmtList[0]
  argTypeSection.expectKind nnkTypeSection

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

  let typeSection = nnkTypeSection.newTree()
  var propertyTypeIdents: array[4, NimNode]
  for i,propertiesSeq in propertiesSequences:
    let typeDef = structOfArraysTypeDef(
      ident($name.ident & propertyCategoryNamesUC[i] & "Properties"),
      propertiesSeq
    )
    typeSection.add typeDef
    propertyTypeIdents[i] = typeDef[0]

  let
    vertexPropertiesTypeIdent   = propertyTypeIdents[0]
    facePropertiesTypeIdent     = propertyTypeIdents[1]
    edgePropertiesTypeIdent     = propertyTypeIdents[2]
    halfedgePropertiesTypeIdent = propertyTypeIdents[3]

  result = newStmtList()

  result.add typeSection

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

  result.add quote do:
    iterator vertexRefs*(mesh : var `name`) : `name`.VertexRef =
      var vertexRef : `name`.VertexRef
      vertexRef.mesh = mesh.addr
      for i in 0 .. high(mesh.vertices):
        vertexRef.handle = VertexHandle(i)
        yield(vertexRef)

    iterator faceRefs*(mesh : var `name`) : `name`.FaceRef =
      var faceRef : `name`.FaceRef
      faceRef.mesh = mesh.addr
      for i in 0 .. high(mesh.faces):
        faceRef.handle = FaceHandle(i)
        yield(faceRef)

    iterator edgeRefs*(mesh : var `name`) : `name`.EdgeRef =
      var edgeRef : `name`.EdgeRef
      edgeRef.mesh = mesh.addr
      for i in 0 .. high(mesh.edges):
        edgeRef.handle = EdgeHandle(i)
        yield(edgeRef)

    iterator halfedgeRefs*(mesh : var `name`) : `name`.HalfedgeRef =
      var halfedgeRef : `name`.HalfedgeRef
      halfedgeRef.mesh = mesh.addr
      for i in 0 .. high(mesh.edges):
        halfedgeRef.handle = HalfedgeHandle(i*2)
        yield(halfedgeRef)
        halfedgeRef.handle = HalfedgeHandle(i*2+1)
        yield(halfedgeRef)


  let argSym = genSym(nskParam, "mesh")
  var initProc = quote do:
    proc init*(`argSym`: var `name`): void =
      ## initializes all ``seq`` member types to empty sequences.
      `argSym`.edges.newSeq(0)
      `argSym`.faces.newSeq(0)
      `argSym`.vertices.newSeq(0)

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

      let call = quote do:
        `argSym`.`propertiesName`.`name`.newSeq(0)

      initProc.body.add call

  echo initProc.repr
  result.add initProc

  result.add quote do:
    meshTypeMethodsTemplate(`name`)

  #echo result.repr
  result = newCall(bindSym"debugAst", result)
