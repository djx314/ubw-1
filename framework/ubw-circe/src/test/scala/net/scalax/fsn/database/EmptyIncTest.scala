package net.scalax.ubw.database.test

import net.scalax.ubw.slick.helpers.SlickUtils
import org.scalatest._
import org.scalatest.concurrent.ScalaFutures

class ShapeTest extends FlatSpec
    with Matchers
    with EitherValues
    with ScalaFutures
    with BeforeAndAfterAll {

  import slick.jdbc.H2Profile.api._

  "shape count" should "return 0 when input a empty shape" in {
    def emptyShapeGen[S, D, T](repLike: S)(implicit implShape: Shape[FlatShapeLevel, S, D, T]) = implShape
    val emptyShape = emptyShapeGen(((), ((), ()), ()))
    SlickUtils.isShapeEmpty(emptyShape) shouldBe true
    val literalShape = emptyShapeGen(((), LiteralColumn(4), ()) -> LiteralColumn(3))
    SlickUtils.shapeLength(literalShape) shouldBe 2
    val constShape = emptyShapeGen(((), 4, ()) -> 3)
    SlickUtils.shapeLength(constShape) shouldBe 0
  }

}