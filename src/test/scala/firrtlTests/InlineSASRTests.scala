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
import java.io.StringWriter
import firrtl.passes.InlineSASR
import firrtl.{Transform, TransformResult, Circuit}
import firrtl.Annotations.AnnotationMap

/**
 * InlineSASR is a Pass, we wrap it in a transform to test it.
 * [S|M]A[S|M]R means [Single|Multiple] Assignment [Single|Multiple] Read
 */
class InlineSASRTransform extends Transform {
   def execute (circuit: Circuit, annotations: AnnotationMap): TransformResult = {
      TransformResult(InlineSASR.run(circuit))
   }
}

/**
 * Tests the following legal candidates for inlining
 * 1) Reference that is ground type, referenced once, a wire, assigned to
 * once, and has a generated name
 * 2) Reference that is ground type, referenced once, a node, and assigned to
 * once, and has a generated name
 *
 * Tests the following illegal candidates for inlining
 * 1) Bundle-type reference
 * 2) Register
 * 3) Wire read from twice
 * 4) Wire assigned to twice
 */
class InlineSASRSpec extends HighTransformSpec with Matchers  {
   val transform = new InlineSASRTransform()
   // =============================
   "The SASR ground-typed wire T_0" should "be inlined" in {
      val input =
"""circuit Top :
  module Top :
    output y : UInt<5>
    wire T_0:UInt<5>
    T_0 <= UInt(0)
    y <= T_0
"""
      val check =
"""circuit Top :
  module Top :
    output y : UInt<5>
    skip
    skip
    y <= UInt(0)
"""
      execute(new StringWriter(), new AnnotationMap(Seq.empty), input, check)
   }
   // =============================
   "The SASR ground-typed node T_0" should "be inlined" in {
      val input =
"""circuit Top :
  module Top :
    output y : UInt<5>
    node T_0 = UInt(0)
    y <= T_0
"""
      val check =
"""circuit Top :
  module Top :
    output y : UInt<5>
    skip
    y <= UInt(0)
"""
      execute(new StringWriter(), new AnnotationMap(Seq.empty), input, check)
   }
   // =============================
   "The SASR bundle-typed node T_0" should "not be inlined" in {
      val input =
"""circuit Top :
  module Top :
    input x : {a:UInt<5>, b:UInt<5>}
    output y : {a:UInt<5>, b:UInt<5>}
    node T_0 = x
    y <= T_0
"""
      val check =
"""circuit Top :
  module Top :
    input x : {a:UInt<5>, b:UInt<5>}
    output y : {a:UInt<5>, b:UInt<5>}
    node T_0 = x
    y <= T_0
"""
      execute(new StringWriter(), new AnnotationMap(Seq.empty), input, check)
   }
   // =============================
   "The SASR ground-typed register T_0" should "not be inlined" in {
      val input =
"""circuit Top :
  module Top :
    input clk:Clock
    output y : UInt<5>
    reg T_0 : UInt<5>, clk with :
      reset => (UInt(0), T_0)
    T_0 <= UInt(0)
    y <= T_0
"""
      val check =
"""circuit Top :
  module Top :
    input clk:Clock
    output y : UInt<5>
    reg T_0 : UInt<5>, clk with :
      reset => (UInt(0), T_0)
    T_0 <= UInt(0)
    y <= T_0
"""
      execute(new StringWriter(), new AnnotationMap(Seq.empty), input, check)
   }
   // =============================
   "The SAMR ground-typed wire T_0" should "not be inlined" in {
      val input =
"""circuit Top :
  module Top :
    output y : UInt<5>
    output z : UInt<5>
    wire T_0 : UInt<5>
    T_0 <= UInt(0)
    y <= T_0
    z <= T_0
"""
      val check =
"""circuit Top :
  module Top :
    output y : UInt<5>
    output z : UInt<5>
    wire T_0 : UInt<5>
    T_0 <= UInt(0)
    y <= T_0
    z <= T_0
"""
      execute(new StringWriter(), new AnnotationMap(Seq.empty), input, check)
   }
   // =============================
   "The MASR ground-typed wire T_0" should "not be inlined" in {
      val input =
"""circuit Top :
  module Top :
    output y : UInt<5>
    wire T_0 : UInt<5>
    T_0 <= UInt(0)
    T_0 <= UInt(1)
    y <= T_0
"""
      val check =
"""circuit Top :
  module Top :
    output y : UInt<5>
    wire T_0 : UInt<5>
    T_0 <= UInt(0)
    T_0 <= UInt(1)
    y <= T_0
"""
      execute(new StringWriter(), new AnnotationMap(Seq.empty), input, check)
   }
}
