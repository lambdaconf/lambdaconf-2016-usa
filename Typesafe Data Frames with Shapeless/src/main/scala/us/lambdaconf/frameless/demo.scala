package us.lambdaconf.frameless

import shapeless.ops.hlist.{Intersection, Union}
import shapeless.record.Record
import shapeless.test.illTyped

import scala.collection.immutable.ListMap

case class Person(name: String, age: Int)

case class Flight(person: Person, from: String, to: String)

object demo extends App {

  val typed = new TypedDataset[Person]

  // 1. Safe column referencing
  typed.col('name)
  typed.col('age)

  val _name: TypedColumn[Person, String] = typed.col[String]('name)
  val _age: TypedColumn[Person, Int] = typed.col[Int]('age)

  // doesn't compile because specified type is invalid
  illTyped("typed.col[Int]('name)")

  // doesn't compile because column doesn't exist
  illTyped("typed.col('gender)")

  // 2. JSON encoder derivation

  // derives encoders for case classes with primitive fields
  val person = Person("Jack", 42)
  val personJson = JObject(ListMap(
    "name" -> JString("Jack"),
    "age" -> JInt(42)
  ))

  require(JEncoder.of[Person].enc(person) == personJson)

  // as well as for case classes with case classes as fields
  val flight = Flight(person, "JFK", "DIA")
  val flightJson = JObject(ListMap(
    "person" -> personJson,
    "from" -> JString("JFK"),
    "to" -> JString("DIA")
  ))

  require(JEncoder.of[Flight].enc(flight) == flightJson)

  // 3. Mirroring value-level operations to type-level

  // selectMany is the same as select, but works with arbitrary arity
  val d1: TypedDataset[(String, Int)] = typed.select(_name, _age)
  val d2: TypedDataset[(String, Int)] = typed.selectMany(_name, _age)

  // type-level union
  implicitly[
    Intersection.Aux[
      Record.`'name -> String, 'age  -> Int`.T,
      Record.`'name -> String, 'address -> String`.T,
      Record.`'name -> String`.T
    ]
  ]

  // type-level intersection
  implicitly[
    Union.Aux[
      Record.`'name -> String, 'age  -> Int`.T,
      Record.`'name -> String, 'address -> String`.T,
      Record.`'name -> String, 'age -> Int, 'address -> String`.T
    ]
  ]
}
