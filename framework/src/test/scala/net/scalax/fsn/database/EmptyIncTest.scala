package net.scalax.fsn.database.test

import net.scalax.fsn.mix.helpers.SlickCRUDImplicits
import net.scalax.fsn.slick.helpers.{FRep, FilterRepImplicitHelper, SlickUtils}
import net.scalax.fsn.slick.model.{RWProperty, SlickParam}
import org.h2.jdbcx.JdbcDataSource
import org.scalatest._
import org.scalatest.concurrent.ScalaFutures
import org.slf4j.LoggerFactory
import slick.ast.{CompiledStatement, ProductNode, Pure, ResultSetMapping}
import slick.jdbc.PositionedResult
import slick.lifted.{ProductNodeShape, RepShape}

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

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