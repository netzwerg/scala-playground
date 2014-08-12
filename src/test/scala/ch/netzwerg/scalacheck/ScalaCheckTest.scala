package ch.netzwerg.scalacheck

import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Properties}

object ScalaCheckTest extends Properties("ScalaCheck Playground") {

  property("startsWith") = forAll { (a: String, b: String) =>
    (a + b).startsWith(a)
  }

  property("concatenate") = forAll { (a: String, b: String) =>
    (a + b).length >= a.length && (a + b).length >= b.length
  }

  property("substring") = forAll { (a: String, b: String, c: String) =>
    (a + b + c).substring(a.length, a.length + b.length) == b
  }

  val rahelsFaves = Gen.oneOf(9, 39, 99)

  property("Rahel's favourite numbers") = forAll(rahelsFaves) { (x: Int) =>
    x.toString.contains("9")
  }

}