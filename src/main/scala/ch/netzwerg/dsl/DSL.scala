package ch.netzwerg.dsl

object DSL extends App {

  object los {
    def apply(body: => Unit) = new MachBody(body)
  }

  def schreib(string: String) = println(string)

  class MachBody(body: => Unit) {

    def hopplaschorsch = body

    def genau(times: Int) = new Times(times, body)

  }

  class Times(times: Int, body: => Unit) {
    def mal = {
      (1 to times) foreach (_ => body)
    }
  }

  los {
    schreib("hallo")
  } hopplaschorsch

  los {
    schreib("mist")
  } genau 5 mal

}