package net.scalax.fsn.core

import shapeless._
import scala.language.implicitConversions

trait FShape[In, Out] {

  def encode(in: In): Out

}

object FShape {

  implicit def fRepLikeExtensionMethod[I <: HList, J, JO, K <: HList, KO <: HList]
  (implicit headCons: I <:< (J :: K), jShape: FShape[J, JO], kShape: FShape[K, KO])
  : FShape[I, JO :: KO] = {
    new FShape[I, JO :: KO] {
      override def encode(in: I): JO :: KO = {
        val jIn :: kIn = headCons(in)
        jShape.encode(jIn) :: kShape.encode(kIn)
      }
    }
  }

  implicit def hNilFShapeGen: FShape[HNil, HNil] = {
    new FShape[HNil, HNil] {
      def encode(in: HNil): HNil = {
        in
      }
    }
  }

}