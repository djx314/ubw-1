package net.scalax.fsn.mix.slickbase

import net.scalax.fsn.mix.helpers.SlickCRUDImplicits
import net.scalax.fsn.slick.helpers.FilterRepImplicitHelper
import slick.lifted._
import slick.relational.RelationalProfile

trait SqlRepImplicits extends FilterRepImplicitHelper with SlickCRUDImplicits