package us.lambdaconf.frameless

import shapeless.ops.hlist.{Comapped, Tupler}
import shapeless.{HList, ProductArgs, Witness}

class Column
class TypedColumn[T, U]

class DataFrame {
  def col(name: String): Column = new Column
}

class TypedDataset[A] {
  def col[U](c: Witness)(
    implicit e: Exists[A, c.T, U]
  ): TypedColumn[A, U] = new TypedColumn[A, U]

  def select[U1, U2](
    c1: TypedColumn[A, U1],
    c2: TypedColumn[A, U2]
  ): TypedDataset[(U1, U2)] = selectMany(c1, c2)

  def select[U1, U2, U3](
    c1: TypedColumn[A, U1],
    c2: TypedColumn[A, U2],
    c3: TypedColumn[A, U3]
  ): TypedDataset[(U1, U2, U3)] = selectMany(c1, c2, c3)

  object selectMany extends ProductArgs {
    def applyProduct[U <: HList, Out <: HList](
      cols: U
    )(
      implicit
      cm: Comapped.Aux[U, TypedColumn[A, ?], Out],
      tupler: Tupler[Out]
    ): TypedDataset[tupler.Out] = new TypedDataset[tupler.Out]
  }
}
