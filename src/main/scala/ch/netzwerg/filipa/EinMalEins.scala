package ch.netzwerg.filipa

import java.io.EOFException
import scala.util.Random

object EinMalEins extends App {

  val R = new Random()
  val RECHNUNGEN = 3.erReihe ++ 5.erReihe ++ 6.erReihe

  implicit class IntWithReihe(faktor: Int) {
    def erReihe = for (i <- 0 to 10) yield {
      val rechnung = i + " x " + faktor
      val loesung = i * faktor
      rechnung -> loesung
    }
  }

  while (true) {
    val randInt = R.nextInt(RECHNUNGEN.size)
    val (rechnung, loesung) = RECHNUNGEN(randInt)

    print("\nWas gibt " + rechnung + "? ")
    val i = try {
      Console.readInt()
    } catch {
      case e: NumberFormatException => 42
      case efo: EOFException => System.exit(0)
    }
    if (i == loesung) {
      Console.println("RICHTIG")
    } else {
      Console.println("Leider falsch, es gibt " + loesung)
    }
  }

}