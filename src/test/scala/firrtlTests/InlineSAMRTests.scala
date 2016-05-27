/*
Copyright (c) 2014 - 2016 The Regents of the University of
California (Regents). All Rights Reserved.  Redistribution and use in
source and binary forms, with or without modification, are permitted
provided that the following conditions are met:
   * Redistributions of source code must retain the above
     copyright notice, this list of conditions and the following
     two paragraphs of disclaimer.
   * Redistributions in binary form must reproduce the above
     copyright notice, this list of conditions and the following
     two paragraphs of disclaimer in the documentation and/or other materials
     provided with the distribution.
   * Neither the name of the Regents nor the names of its contributors
     may be used to endorse or promote products derived from this
     software without specific prior written permission.
IN NO EVENT SHALL REGENTS BE LIABLE TO ANY PARTY FOR DIRECT, INDIRECT,
SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING LOST PROFITS,
ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF
REGENTS HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
REGENTS SPECIFICALLY DISCLAIMS ANY WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE. THE SOFTWARE AND ACCOMPANYING DOCUMENTATION, IF
ANY, PROVIDED HEREUNDER IS PROVIDED "AS IS". REGENTS HAS NO OBLIGATION
TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
MODIFICATIONS.
*/

package firrtlTests

import org.scalatest.Matchers
import java.io.{StringWriter,Writer}
import firrtl.passes.InlineSAMR
import firrtl.{Transform, TransformResult, Circuit}
import firrtl.Annotations.AnnotationMap

/**
 * InlineSAMR is a Pass, we wrap it in a transform to test it.
 * [S|M]A[S|M]R is [Single|Multiple] Assignment [Single|Multiple] Read
 */
class InlineSAMRTransform extends Transform {
  def execute (circuit: Circuit, annotations: AnnotationMap): TransformResult = {
    TransformResult(InlineSAMR.run(circuit))
  }
}


/**
 * Tested correct cases for inlining:
 *  1) SAMR ground-typed wire with generated name and generates no hardware
 *  2) SAMR ground-typed node with generated name and generates no hardware
 *
 * Tested incorrect cases for inlining:
 *  1) MAMR ground-typed wire with generated name and generates no hardware
 *  2) SAMR bundle-typed wire with generated name and generates no hardware
 *  3) SAMR ground-typed register with generated name and generates no hardware
 *  4) SAMR ground-typed wire with normal name and generates no hardware
 *  5) SAMR ground-typed wire with generated name and generates hardware
 */
class InlineSAMRSpec extends HighTransformSpec with Matchers  {
  val transform = new InlineSAMRTransform()
  // 1) =============================
  "SAMR ground-typed wire T0" should "be inlined" in {
    val input =
"""circuit Top :
  module Top :
    input x : UInt<1>
    output y : UInt<1>
    output z : UInt<1>
    wire T_0 : UInt<1>
    T_0 <= x
    y <= T_0
    z <= T_0
"""
    val check =
"""circuit Top :
  module Top :
    input x : UInt<1>
    output y : UInt<1>
    output z : UInt<1>
    skip
    skip
    y <= x
    z <= x
"""
    execute(new StringWriter(), new AnnotationMap(Seq.empty), input, check)
  }

  // 2) =============================
  "SAMR ground-typed node T0" should "be inlined" in {
    val input =
"""circuit Top :
  module Top :
    input x : UInt<1>
    output y : UInt<1>
    output z : UInt<1>
    node T_0 = x
    y <= T_0
    z <= T_0
"""
    val check =
"""circuit Top :
  module Top :
    input x : UInt<1>
    output y : UInt<1>
    output z : UInt<1>
    skip
    y <= x
    z <= x
"""
    execute(new StringWriter(), new AnnotationMap(Seq.empty), input, check)
  }

  // 1) =============================
  "MAMR ground-typed wire T0" should "not be inlined" in {
    val input =
"""circuit Top :
  module Top :
    input a : UInt<1>
    input b : UInt<1>
    output y : UInt<1>
    output z : UInt<1>
    wire T_0 : UInt<1>
    T_0 <= a
    T_0 <= b
    y <= T_0
    z <= T_0
"""
    val check =
"""circuit Top :
  module Top :
    input a : UInt<1>
    input b : UInt<1>
    output y : UInt<1>
    output z : UInt<1>
    wire T_0 : UInt<1>
    T_0 <= a
    T_0 <= b
    y <= T_0
    z <= T_0
"""
    execute(new StringWriter(), new AnnotationMap(Seq.empty), input, check)
  }

  // 2) =============================
  "SAMR bundle-typed wire T0" should "not be inlined" in {
    val input =
"""circuit Top :
  module Top :
    input x : {a : UInt<1>}
    output y : {a : UInt<1>}
    output z : {a : UInt<1>}
    wire T_0 : {a : UInt<1>}
    T_0 <= x
    y <= T_0
    z <= T_0
"""
    val check =
"""circuit Top :
  module Top :
    input x : {a : UInt<1>}
    output y : {a : UInt<1>}
    output z : {a : UInt<1>}
    wire T_0 : {a : UInt<1>}
    T_0 <= x
    y <= T_0
    z <= T_0
"""
    execute(new StringWriter(), new AnnotationMap(Seq.empty), input, check)
  }

  // 3) =============================
  "SAMR ground-typed register T0" should "not be inlined" in {
    val input =
"""circuit Top :
  module Top :
    input clk : Clock
    input reset : UInt<1>
    input x : UInt<1>
    output y : UInt<1>
    output z : UInt<1>
    reg T_0 : UInt<1>, clk with: (reset => (reset, UInt(0)))
    T_0 <= x
    y <= T_0
    z <= T_0
"""
    val check =
"""circuit Top :
  module Top :
    input clk : Clock
    input reset : UInt<1>
    input x : UInt<1>
    output y : UInt<1>
    output z : UInt<1>
    reg T_0 : UInt<1>, clk with: (reset => (reset, UInt(0)))
    T_0 <= x
    y <= T_0
    z <= T_0
"""
    execute(new StringWriter(), new AnnotationMap(Seq.empty), input, check)
  }

  // 4) =============================
  "SAMR ground-typed wire name" should "not be inlined" in {
    val input =
"""circuit Top :
  module Top :
    input x : UInt<1>
    output y : UInt<1>
    output z : UInt<1>
    wire name : UInt<1>
    name <= x
    y <= name
    z <= name
"""
    val check =
"""circuit Top :
  module Top :
    input x : UInt<1>
    output y : UInt<1>
    output z : UInt<1>
    wire name : UInt<1>
    name <= x
    y <= name
    z <= name
"""
    execute(new StringWriter(), new AnnotationMap(Seq.empty), input, check)
  }

  // 5) =============================
  "SAMR ground-typed wire T_0 with hardware" should "not be inlined" in {
    val input =
"""circuit Top :
  module Top :
    input x : UInt<1>
    output y : UInt<1>
    output z : UInt<1>
    wire T_0 : UInt<1>
    T_0 <= not(x)
    y <= T_0
    z <= T_0
"""
    val check =
"""circuit Top :
  module Top :
    input x : UInt<1>
    output y : UInt<1>
    output z : UInt<1>
    wire T_0 : UInt<1>
    T_0 <= not(x)
    y <= T_0
    z <= T_0
"""
    execute(new StringWriter(), new AnnotationMap(Seq.empty), input, check)
  }
}
