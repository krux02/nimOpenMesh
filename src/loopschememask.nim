#[ ========================================================================= *
 *                                                                           *
 *                               OpenMesh                                    *
 *           Copyright (c) 2001-2015, RWTH-Aachen University                 *
 *           Department of Computer Graphics and Multimedia                  *
 *                          All rights reserved.                             *
 *                            www.openmesh.org                               *
 *                                                                           *
 *---------------------------------------------------------------------------*
 * This file is part of OpenMesh.                                            *
 *---------------------------------------------------------------------------*
 *                                                                           *
 * Redistribution and use in source and binary forms, with or without        *
 * modification, are permitted provided that the following conditions        *
 * are met:                                                                  *
 *                                                                           *
 * 1. Redistributions of source code must retain the above copyright notice, *
 *    this list of conditions and the following disclaimer.                  *
 *                                                                           *
 * 2. Redistributions in binary form must reproduce the above copyright      *
 *    notice, this list of conditions and the following disclaimer in the    *
 *    documentation and/or other materials provided with the distribution.   *
 *                                                                           *
 * 3. Neither the name of the copyright holder nor the names of its          *
 *    contributors may be used to endorse or promote products derived from   *
 *    this software without specific prior written permission.               *
 *                                                                           *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS       *
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED *
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A           *
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER *
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,  *
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,       *
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR        *
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF    *
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING      *
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS        *
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.              *
 *                                                                           *
 * ========================================================================= ]#

import math

# note from Arne, big WTF am I porting here

#[ implements cache for the weights of the original Loop scheme
    supported:
      - vertex projection rule on the next level
      - vertex projection rule on the limit surface
      - vertex projection rule on the k-th (level) step (Barthe, Kobbelt'2003)
      - vertex tangents on the limit surface
]#

const cache_size = 128

type Scalar = float32

proc compute_proj_weight(valence: int): Scalar =
  let
    denom =   3 + 2 * cos(Scalar(Tau) / Scalar(valence))
    weight = (64 * Scalar(valence)) / (40 - denom*denom) - Scalar(valence)
  return weight


proc compute_limit_weight(valence: int): Scalar =
  var proj_weight = compute_proj_weight(valence)
  proj_weight = proj_weight / (proj_weight + Scalar(valence)) # normalize the proj_weight
  let weight = (3.0 / 8.0)/(1.0 - proj_weight + (3.0/8.0));
  return Scalar(weight)

proc compute_step_weight(valence: int): Scalar =
  var proj_weight = compute_proj_weight(valence)
  proj_weight = proj_weight / (proj_weight + Scalar(valence)) # normalize the proj_weight
  let weight = proj_weight - 3.0 / 8.0
  return Scalar(weight)


proc compute_tang0_weight(valence, ver_id: int): Scalar =
  return cos(Scalar(Tau) * Scalar(ver_id) / Scalar(valence))

proc compute_tang1_weight(valence, ver_id: int): Scalar =
  return sin(Scalar(Tau) * Scalar(ver_id) / Scalar(valence))


# cache weights

var
  proj_weights:  array[cache_size, Scalar]
  limit_weights: array[cache_size, Scalar]
  step_weights:  array[cache_size, Scalar]
  tang0_weights: array[cache_size, seq[Scalar]]
  tang1_weights: array[cache_size, seq[Scalar]]

proj_weights[0] = 1
for k in 1 ..< cache_size:
  proj_weights[k] = compute_proj_weight(k);
  limit_weights[k] = compute_limit_weight(k);
  step_weights[k] = compute_step_weight(k);
  tang0_weights[k].newSeq(k) # WTF resize in inner loop?
  tang1_weights[k].newSeq(k) # TODO what is real resize function?

  for i in 0 ..< k:
    tang0_weights[k][i] = compute_tang0_weight(k,i);
    tang1_weights[k][i] = compute_tang1_weight(k,i);


proc proj_weight*(valence: int): Scalar =
  assert(valence < cache_size) # really assert?
  return proj_weights[valence];

proc limit_weight*(valence: int): Scalar =
  assert(valence < cache_size) # really assert?
  return limit_weights[valence];

proc step_weight*(valence, step: int): Scalar =
  assert(valence < cache_size) # really assert?
  # :-( I have to cast
  # can be precomputed
  return pow(step_weights[valence], Scalar(step))

proc tang0_weight*(valence, vertex_id: int): Scalar =
  assert(valence < cache_size)
  assert(vertex_id < valence)
  return tang0_weights[valence][vertex_id]


proc tang1_weight*(valence, vertex_id: int): Scalar =
  assert(valence < cache_size)
  assert(vertex_id < valence)
  return tang1_weights[valence][vertex_id]


proc dump*(max_valency: int = cache_size - 1): void =
  assert(max_valency < cache_size)
  for i in 0 .. max_valency:
    stdout.write "(", i, " : ", proj_weight(i), ", ", limit_weight(i)
    stdout.write ", ", step_weight(i,1), "), "
  echo()
