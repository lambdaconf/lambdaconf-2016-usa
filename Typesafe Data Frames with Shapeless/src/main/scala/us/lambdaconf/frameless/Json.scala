package us.lambdaconf.frameless

import shapeless._
import shapeless.labelled.FieldType

import scala.collection.immutable.ListMap

sealed trait Json

case class JString(value: String) extends Json
case class JInt(value: Int) extends Json
case class JObject(map: ListMap[String, Json]) extends Json

trait JEncoder[A] {
  def enc(value: A): Json
}

trait JObjectEncoder[A] {
  def enc(value: A): JObject
}

object JObjectEncoder {
  def apply[A](f: A => JObject): JObjectEncoder[A] = new JObjectEncoder[A] { def enc(value: A): JObject = f(value) }
  def of[A: JObjectEncoder]: JObjectEncoder[A] = implicitly[JObjectEncoder[A]]

  implicit def hnilJEncoder: JObjectEncoder[HNil] = JObjectEncoder { _ => JObject(ListMap.empty) }

  implicit def deriveCons[
    K <: Symbol : Witness.Aux,
    V : JEncoder,
    T <: HList : JObjectEncoder
  ]: JObjectEncoder[FieldType[K, V] :: T] = JObjectEncoder {
    case head :: tail =>
      val tailObject = JObjectEncoder.of[T].enc(tail)
      val fieldName = implicitly[Witness.Aux[K]].value.name
      JObject(ListMap(fieldName -> JEncoder.of[V].enc(head)) ++ tailObject.map)
  }
}

object JEncoder {
  def apply[A](f: A => Json): JEncoder[A] = new JEncoder[A] { def enc(value: A): Json = f(value) }
  def of[A: JEncoder]: JEncoder[A] = implicitly[JEncoder[A]]

  implicit def stringJEncoder: JEncoder[String] = JEncoder { str => JString(str) }
  implicit def intJEncoder: JEncoder[Int] = JEncoder { int => JInt(int) }

  implicit def deriveLGen[A, L <: HList](
    implicit
    lgen: LabelledGeneric.Aux[A, L],
    jObjectEncoder: JObjectEncoder[L]
  ): JEncoder[A] = JEncoder[A](a => jObjectEncoder.enc(lgen.to(a)))

}
