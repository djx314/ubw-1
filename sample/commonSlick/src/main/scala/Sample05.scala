package net.scalax.fsn.database.test

import io.circe.Json
import io.circe.syntax._
import io.circe.generic.auto._
import net.scalax.fsn.core.FPathImpl
import net.scalax.fsn.json.operation.{ FDefaultAtomicHelper, FPropertyAtomicHelper }
import net.scalax.fsn.slick.helpers.{ FJsonAtomicHelper, FSelectExtAtomicHelper }

import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.implicitConversions
import slick.jdbc.H2Profile.api._

object Sample05 {

  implicit def fPilesOptionImplicit[D](path: FPathImpl[D]): FJsonAtomicHelper[D] with FSelectExtAtomicHelper[D] with FPropertyAtomicHelper[D] with FDefaultAtomicHelper[D] = {
    val path1 = path
    new FJsonAtomicHelper[D] with FSelectExtAtomicHelper[D] with FPropertyAtomicHelper[D] with FDefaultAtomicHelper[D] {
      override val path = path1
    }
  }

}