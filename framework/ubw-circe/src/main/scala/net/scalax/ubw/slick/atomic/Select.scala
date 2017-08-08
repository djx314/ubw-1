package net.scalax.fsn.slick.atomic
import net.scalax.fsn.core.Atomic
/*trait SlickSelect[E] extends Atomic[E] {
  type SourceType
  type SlickType
  type TargetType
  type DataType = E

  val shape: Shape[_ <: FlatShapeLevel, SourceType, SlickType, TargetType]
  val outConvert: SlickType => DataType
  val outCol: SourceType
  val colToOrder: Option[TargetType => ColumnOrdered[_]]
}

case class SSelect[S, D, T, E](
    override val shape: Shape[_ <: FlatShapeLevel, S, D, T],
    override val outConvert: D => E,
    override val outCol: S,
    override val colToOrder: Option[T => ColumnOrdered[_]]
) extends SlickSelect[E] with AtomicPathImpl[E] {
  override type SourceType = S
  override type SlickType = D
  override type TargetType = T
  override type DataType = E

  def order(implicit cv: T => ColumnOrdered[_]): SSelect[S, D, T, E] = {
    this.copy(colToOrder = Option(cv))
  }

  override val atomics: List[Atomic[E]] = this :: Nil
}

trait OrderNullsLast[E] extends Atomic[E] {
  val isOrderNullsLast: Boolean
}

trait InRetrieve[E] extends Atomic[E] {
  val isInRetrieve: Boolean
}

trait OrderTargetName[E] extends Atomic[E] {
  val orderTargetName: String
}*/

trait InRetrieve[E] extends Atomic[E] {
  val isInRetrieve: Boolean
}

trait DefaultDesc[E] extends Atomic[E] {
  val isDefaultDesc: Boolean
}