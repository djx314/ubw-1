package net.scalax.fsn.slick.operation

object CreateOrUpdateOperation {
  /*def parseInsert(
    insertQuerytWrap: List[(Any, SlickQueryBindImpl)],
    columns: List[FColumn]
  )(
    implicit
    ec: ExecutionContext
  ): DBIO[ExecInfo3] = {
    RetrieveOperation.parseInsert(insertQuerytWrap, columns).asTry.flatMap {
      case Success(_) =>
        UpdateOperation.parseInsert(insertQuerytWrap, columns).map { s =>
          ExecInfo(s.effectRows, Nil)
        }
      case s =>
        CreateOperation.parseInsert(insertQuerytWrap, columns).map { s =>
          ExecInfo(s.effectRows, Nil)
        }
    }
  }*/
}