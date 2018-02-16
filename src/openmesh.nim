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

  GBaseMesh*[VertexProps, FaceProps, EdgeProps, HalfedgeProps] = object
    vertices* : seq[Vertex]
    edges*    : seq[Edge]
    faces*    : seq[Face]
    vertexProps*: VertexProps
    faceProps*: FaceProps
    edgeProps*: EdgeProps
    halfedgeProps*: HalfedgeProps

  GVertexRef*[MeshType] = object
    mesh*: ptr MeshType
    handle*: VertexHandle

  GFaceRef*[MeshType] = object
    mesh*: ptr MeshType
    handle*: FaceHandle

  GEdgeRef*[MeshType] = object
    mesh*: ptr MeshType
    handle*: EdgeHandle

  GHalfedgeRef*[MeshType] = object
    mesh*: ptr MeshType
    handle*: HalfedgeHandle


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

macro hasProperty(tpe: typedesc; propertyType, ident: untyped): bool =
  block b:
    # Sym(MyMeshType) -> typedesc[MyMeshType] -> MyMeshType -> objectTy(...)
    let impl = tpe.getTypeInst[1].getTypeImpl
    for identDefs in impl[2]:
      identDefs.expectKind nnkIdentDefs
      let name = $identDefs[0]
      if eqIdent(propertyType, name):
        for innerIdentDefs in identDefs[1].symbol.getImpl[2][2]:
          if innerIdentDefs[0].ident == ident.ident:
            result = newLit(true)
            return

  result = newLit(false)

macro propertyType(tpe: typedesc; propertyType, ident: untyped): untyped =
  block b:
    # Sym(MyMeshType) -> typedesc[MyMeshType] -> MyMeshType -> objectTy(...)
    let impl = tpe.getTypeInst[1].getTypeImpl
    for identDefs in impl[2]:
      identDefs.expectKind nnkIdentDefs
      let name = $identDefs[0]
      if eqIdent(propertyType, name):
        for innerIdentDefs in identDefs[1].symbol.getImpl[2][2]:
          if innerIdentDefs[0].ident == ident.ident:
            result = innerIdentDefs[1][1]
            return
  error("in propertyType failed", callsite())

template vertexPropertyType*(tpe: typedesc; ident: untyped): typedesc =
  propertyType(MyMeshType, vertexProps, point)

template facePropertyType*(tpe: typedesc; ident: untyped): typedesc =
  propertyType(MyMeshType, faceProps, point)

template edgePropertyType*(tpe: typedesc; ident: untyped): typedesc =
  propertyType(MyMeshType, edgeProps, point)

template halfedgePropertyType*(tpe: typedesc; ident: untyped): typedesc =
  propertyType(MyMeshType, halfedgeProps, point)

template hasVertexProperty*(tpe: typedesc, ident: untyped): bool =
  hasProperty(tpe, vertexProps, ident)

template hasFaceProperty*(tpe: typedesc, ident: untyped): bool =
  hasProperty(tpe, faceProps, ident)

template hasHalfedgeProperty*(tpe: typedesc, ident: untyped): bool =
  hasProperty(tpe, halfedgeProps, ident)

template hasEdgeProperty*(tpe: typedesc, ident: untyped): bool =
  hasProperty(tpe, edgeProps, ident)

proc structOfArraysTypeDef(name: NimNode, members: seq[tuple[name, typ: NimNode]]) : NimNode =

  var mappedMembers = newSeq[tuple[name,typ:NimNode]](len(members))

  for i,tup in members:
    mappedMembers[i].name = tup.name
    mappedMembers[i].typ  = nnkBracketExpr.newTree(ident("seq"), tup.typ)

  let recList = newNimNode(nnkRecList)
  for tup in mappedMembers:
    recList.add nnkIdentDefs.newTree(tup.name, tup.typ, newEmptyNode())

  result = quote do:
    type `name` = object
  result.expectKind nnkTypeSection
  result = result[0] # strip type section
  result[2][2] = recList

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
  var debug = false

  name.expectKind nnkIdent
  argStmtList.expectKind nnkStmtList

  if argStmtList[0].kind == nnkIdent:
    if eqIdent(argStmtList[0], "debug"):
      debug = true
    else:
      error("undefined identifier in DSL", argStmtList[0])
  else:
    argStmtList[0].expectKind nnkTypeSection

  let argTypeSection = argStmtList[^1]
  argTypeSection.expectKind nnkTypeSection

  let
    propertyCategoryNames    = ["vertex", "face", "edge", "halfedge"]
    propertyCategoryNamesUC  = ["Vertex", "Face", "Edge", "Halfedge"]
    propertyTypeIdents = [
      ident($name.ident &   "VertexProps"),
      ident($name.ident &     "FaceProps"),
      ident($name.ident &     "EdgeProps"),
      ident($name.ident & "HalfedgeProps")
    ]

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


  let typeExpr = nnkBracketExpr.newTree(
    bindSym"GBaseMesh",
    propertyTypeIdents[0],
    propertyTypeIdents[1],
    propertyTypeIdents[2],
    propertyTypeIdents[3]
  )

  let typeSection = quote do:
    type
      `name`* = `typeExpr`

  for i, propertiesSeq in propertiesSequences:
    typeSection.add structOfArraysTypeDef(
      propertyTypeIdents[i],
      propertiesSeq
    )

  result = newStmtList()
  result.add typeSection

  result.add newCall(bindSym"meshRefTypesTemplate", name)

  # create walker types

  let argSym = genSym(nskParam, "mesh")
  let initProc = quote do:
    proc init*(`argSym`: var `name`): void =
      ## initializes all ``seq`` member types to empty sequences.
      `argSym`.edges.newSeq(0)
      `argSym`.faces.newSeq(0)
      `argSym`.vertices.newSeq(0)

  for i, propertiesSeq in propertiesSequences:
    let propertiesName = ident(propertyCategoryNames[i] & "Props")
    for tup in propertiesSeq:
      let name = tup.name
      let call = quote do:
        `argSym`.`propertiesName`.`name`.newSeq(0)
      initProc.body.add call

  result.add initProc


  # create property accessors from walkers
  let walkerProcs = newStmtList()
  for i, propertiesSeq in propertiesSequences:
    let
      propertiesName = ident(propertyCategoryNames[i] & "Props")
      refIdent = nnkBracketExpr.newTree(ident("G" & propertyCategoryNamesUC[i] & "Ref"), name)

    for tup in propertiesSeq:
      let
        typ = tup.typ
        name = tup.name

      var nameStr = $name.ident
      # first letter to upper case without dependency
      if 'a' <= nameStr[0] and nameStr[0] <= 'z':
        nameStr[0] = char(nameStr[0].int - 32)
      let accessorIdent = ident("prop" & nameStr)

      walkerProcs.add quote do:
        proc `accessorIdent`*(walker: `refIdent`): var `typ` =
          walker.mesh.`propertiesName`.`name`[walker.handle.int]
  result.add walkerProcs

  result.add quote do:
    meshTypeMethodsTemplate(`name`)


  if debug:
    echo "################################################################################"
    echo "untyped: "
    echo result.repr
    echo "################################################################################"
    result = newCall(bindSym"debugAst", result)
