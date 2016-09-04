import macros

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

# parameter to control export
