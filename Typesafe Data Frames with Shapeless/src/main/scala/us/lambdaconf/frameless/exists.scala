package us.lambdaconf.frameless

import shapeless.ops.record.Selector
import shapeless.{HList, LabelledGeneric}

import scala.annotation.implicitNotFound

/** Evidence that type `T` has column `K` *with type `V`.  */
@implicitNotFound(msg = "No column ${K} of type ${V} in ${T}")
trait Exists[T, K, V]

object Exists {
  implicit def derive[T, H <: HList, K, V](
    implicit
    lgen: LabelledGeneric.Aux[T, H],
    selector: Selector.Aux[H, K, V]
  ): Exists[T, K, V] = new Exists[T, K, V] {}
}
