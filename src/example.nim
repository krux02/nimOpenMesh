import glm

type
  ObjData = object
    # VertexData

    geometricVertices: seq[Vec4d]      # v
    textureVertices:   seq[Vec3d]      # vt
    vertexNormals:     seq[Vec3d]      # vn
    parameterSpaceVertices: seq[Vec3d] # vp

    ## Free-form curve/surface attributes

    # rational or non-rational forms of curve or surface type: basis
    # matrix, Bezier, B-spline, Cardinal, Taylor (cstype)

    degree: float                    # deg
    basisMatrix: float               # bmat
    stepSize: float                  # step

    # Elements

    points:  seq[int32] # p

    # l
    lineVertices: seq[tuple[v, vt: int32]]
    lineLengths: seq[int32]
    # special case to handle lines with exact 2 vertices
    strokeLines: seq[array[2, tuple[v, vt: int32]]]

    # f
    # a face from an obj file is put into faceVertices
    # the amount of vertices a face has is stored in faceLengths
    faceVertices:  seq[tuple[v,vt,vn: int32]]
    faceLengths: seq[int32]
    # special case to handle faces with exact 3 vertices
    triangleFaces: seq[array[3, tuple[v,vt,vn: int32]]]

    curves: float # curv
    curves2D: float # curv2
    surfaces: float # surf

    # Free-Form curve/surface body statements

    parameterValues: float     # param
    outerTrimmingLoop: float   # trim
    innerTrimmingLoop: float   # hole
    specialCurve: float        # scrv
    specialPoint: float        # sp

    # endStatement: end

    # Connectivity between free-form surfaces

    connect: float

    # Grouping

    groupName: string      # g
    smoothingGroup: string # s
    mergingGroup: string   # mg
    objectName: string     # o

    # Display/render attributes

    bevelInterpolation: int               # bevel
    colorInterpolation: int               # c_interp
    dissolveInterpolation: int            # d_interp
    levelOfDetail: int                    # lod
    materialName: string                  # usemtl
    materialLibrary: string               # mtllib
    shadowCasting: string                 # shadow_obj
    rayTracing: string                    # trace_obj
    curveApproximationTechnique: string   # ctech
    surfaceApproximationTechnique: string # stech



# call  filename.ext arg1 arg2 . . .
# csh   (-)command   ## Executes the requested UNIX command.

  ## parsing tokens



  # u v w
  # vp x y z w
  # strncmp

proc init(arg: var ObjData): void =
  arg.geometricVertices.newSeq(0)
  arg.textureVertices.newSeq(0)
  arg.vertexNormals.newSeq(0)
  arg.parameterSpaceVertices.newSeq(0)

  arg.points.newSeq(0)
  arg.faceVertices.newSeq(0)
  arg.faceLengths.newSeq(0)
  arg.triangleFaces.newSeq(0)


import strutils, parseutils


proc parseObj(filename: string): ObjData =
  result.init

  let data = readFile(filename)
  var dataIndex = 0

  var token = newStringOfCap(64)
  var lineIndex   = 1
  var columnIndex = 0

  proc error(arg: string): void =
    let msg = filename & "(" & $lineIndex & ", " & $columnIndex & ") Error: " & arg
    quit(msg)

  proc warning(arg: string): void =
    let msg = filename & "(" & $lineIndex & ", " & $columnIndex & ") Warning: " & arg
    stderr.writeLine(msg)

  proc skipToNextLine(): void =
    let idx = data.find('\l', dataIndex)
    if idx < 0: # line dose not end anymore
      dataIndex = data.len
    else:
      dataIndex = idx + 1

    columnIndex = 0
    lineIndex += 1

  proc skipWhitespace(): void =
    ## skips the whitespace within the Line
    while dataIndex < data.len and data[dataIndex] in {' ', '\t'}:
      dataIndex += 1
      columnIndex += 1

    let i = data.skip("\\\r\l", dataIndex)
    let j = data.skip("\\\l", dataIndex)
    if i > 0 or j > 0:
      columnIndex = 0
      lineIndex += 1
      # only one of them can have a value not equal to 0
      dataIndex += i
      dataIndex += j
      skipWhitespace()

  proc atLineEnd(): bool =
    skipWhitespace()
    data[dataIndex] in {'\r', '\l'}

  proc readFloat(): float =
    skipWhitespace()
    let floatChars = data.parseFloat(result, dataIndex)
    dataIndex   += floatChars
    columnIndex += floatChars
    if floatChars == 0:
      error("expected float")

  proc readFloat(default: float): float =
    skipWhitespace()
    let floatChars = data.parseFloat(result, dataIndex)
    dataIndex   += floatChars
    columnIndex += floatChars
    if floatChars == 0:
      result = default

  proc readInt(): int =
    skipWhitespace()
    let intChars = data.parseInt(result, dataIndex)
    dataIndex   += intChars
    columnIndex += intChars
    if intChars == 0:
      error("expected int")

  proc readInt(default: int): int =
    # Consumen an integer from the input stream. If no int can be found, use `default`
    skipWhitespace()
    let intChars = data.parseInt(result, dataIndex)
    dataIndex   += intChars
    columnIndex += intChars
    if intChars == 0:
      result = default

  proc consumeSlash(): bool =
    ## Ty to read a slash. When found it is consumeed and true is
    ## returned.
    if data[dataIndex] == '/':
      dataIndex   += 1
      columnIndex += 1
      return true
    else:
      return false

  proc readToken(token: var string): void =
    ## result is stored in token variable to avoid string allocation
    skipWhitespace()
    let offset = data.parseUntil(token, Whitespace, dataIndex)
    dataIndex   += offset
    columnIndex += offset

  while dataIndex < data.len:
    readToken(token)
    case token
    of "#": # comment
      # don't do anything with a comment
      discard
    # Vertex data
    of "v": # geometric vertex
      let x = readFloat()
      let y = readFloat()
      let z = readFloat()
      let w = readFloat(1.0)
      let vertex = vec4(x,y,z,w)
      result.geometricVertices.add vertex
    of "vt": # texture vertex
      let u = readFloat()
      let v = readFloat(0.0)
      let w = readFloat(0.0)
      let vertex = vec3(u,v,w)
      result.textureVertices.add vertex
    of "vn": # vertex normal
      let i = readFloat()
      let j = readFloat()
      let k = readFloat()
      let normal = vec3(i,j,k)
      result.vertexNormals.add normal
    of "vp": # parameter space vertex
      let u = readFloat()
      let v = readFloat()
      let w = readFloat(1.0)
      let vertex = vec3(u,v,w)
      result.parameterSpaceVertices.add vertex
    of "cstype":
      # rational or non-rational forms of curve or surface type: basis
      # matrix, Bezier, B-spline, Cardinal, Taylor (cstype)
      warning("unhandled token: " & token)
    of "deg":
      # degree
      warning("unhandled token: " & token)
    of "bmat":
      # basis matrix
      warning("unhandled token: " & token)
    of "step":
      # step size
      warning("unhandled token: " & token)

    # Elements
    of "p":
      # point
      while not atLineEnd():
        var point = int32(readInt())

        if point < 0:
          point += int32(1 + result.geometricVertices.len)

        result.points.add point
    of "l":
      var lineLength = 0
      var stroke: array[2, tuple[v, vt: int32]]
      while not atLineEnd():
        var point : tuple[v,vt: int32]
        point.v = int32(readInt())
        if consumeSlash():
          point.vt = int32(readInt())

        if point.v < 0:
          point.v += int32(1 + result.geometricVertices.len)
        if point.vt < 0:
          point.vt += int32(1 + result.textureVertices.len)

        if lineLength < 2:
          stroke[lineLength] = point
        elif lineLength == 2:
          # give up the idea of using just two vertices for this line
          result.lineVertices.add stroke[0]
          result.lineVertices.add stroke[1]
          result.lineVertices.add point
        else:
          result.lineVertices.add point
        lineLength += 1
      if lineLength < 2:
        error("need at least two vertices for a line")
      elif lineLength == 2:
        result.strokeLines.add stroke
      else:
        result.lineLengths.add int32(lineLength)

    of "f":
      var faceLength = 0
      var triangleFace: array[3, tuple[v,vt,vn: int32]]
      while not atLineEnd():
        var point: tuple[v,vt,vn: int32]
        point.v = int32(readInt())
        if consumeSlash():
          point.vt = int32(readInt(0))
          if consumeSlash():
            point.vn = int32(readInt(0))

        if point.v < 0:
          point.v += int32(1 + result.geometricVertices.len)
        if point.vt < 0:
          point.vt += int32(1 + result.textureVertices.len)
        if point.vn < 0:
          point.vn += int32(1 + result.vertexNormals.len)

        if faceLength < 3:
          triangleFace[faceLength] = point
        elif faceLength == 3:
          # give up the idea of using a triangle
          for point in triangleFace:
            result.faceVertices.add point
          result.faceVertices.add point
        else:
          result.faceVertices.add point
        faceLength += 1

      if faceLength < 3:
        error("need at least 3 vertices per face")
      elif faceLength == 3:
        # we can use our triangle optimization
        result.triangleFaces.add triangleFace
      else:
        # we have to store the length separately
        result.faceLengths.add int8(faceLength)
      #warning("unhandled token: " & token)
    of "curv":
      # curve
      warning("unhandled token: " & token)
    of "curv2":
      # 2D curve
      warning("unhandled token: " & token)
    of "surv":
      # surface
      warning("unhandled token: " & token)

    # Free-Form curve/surface body statements
    of "param":
      # parameter value
      warning("unhandled token: " & token)
    of "trim":
      # outer trimming loop
      warning("unhandled token: " & token)
    of "hole":
      # inner trimming loop
      warning("unhandled token: " & token)
    of "scrv":
      # special curave
      warning("unhandled token: " & token)
    of "sp":
      # special point
      warning("unhandled token: " & token)
    of "end":
      # end statement
      warning("unhandled token: " & token)

    # Connectivity between free-form surfaces
    of "con":
      # connect
      warning("unhandled token: " & token)

    # Grouping
    of "g":
      # group name
      warning("unhandled token: " & token)
    of "s":
      #
      warning("unhandled token: " & token)
    of "mg":
      #
      warning("unhandled token: " & token)
    of "o":
      #
      warning("unhandled token: " & token)

    # Display/render attributes
    of "bevel":
      # bevel interpolation
      warning("unhandled token: " & token)
    of "c_interp":
      # color interpolation
      warning("unhandled token: " & token)
    of "d_interp":
      # dissolve interpolation
      warning("unhandled token: " & token)
    of "lod":
      # level of detail
      warning("unhandled token: " & token)
    of "usemtl":
      # material name
      warning("unhandled token: " & token)
    of "mtllib":
      # material library
      warning("unhandled token: " & token)
    of "shadow_obj":
      # shadow casting
      warning("unhandled token: " & token)
    of "trace_obj":
      # ray tracing
      warning("unhandled token: " & token)
    of "ctech":
      # curve approximation technique
      warning("unhandled token: " & token)
    of "stech":
      # surface approximation technique
      warning("unhandled token: " & token)

    # general statements
    of "call":
      # reads the contents of the specified .obj or .mod file at this location.
      warning("unhandled token: " & token)
    of "csh":
      # Executes the requested UNIX command. If the UNIX command
      # returns an error, the parser flags an error during parsing.
      warning("unhandled token: " & token)

    else:
      error("unknown token: " & token)

    skipToNextLine()

  echo "num triangles: ", result.triangleFaces.len

  #
  #for fv in result.faceVertices:
  #  echo fv

discard parseObj("bunny.obj")
