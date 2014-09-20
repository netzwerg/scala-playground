package ch.netzwerg.akka

import akka.actor._
import akka.event.LoggingReceive
import akka.routing._
import ch.netzwerg.akka.Master.{Work, Start}
import ch.netzwerg.akka.Worker.{Work4You, HeadsUp}

/**
 * Exploring the nuts & bolts of actor pools and broadcasting.
 */
object Broadcasting extends App {
  val system = ActorSystem("akka-broadcasting")
  val master = system.actorOf(Props[Master], "master")
  master ! Start
  master ! Work
}

object Master {
  case object Start
  case object Work
}

class Master extends Actor with ActorLogging {

  import ch.netzwerg.akka.Master.Start

  /** Creates a bunch of worker actors, to which messages will be delivered in a round-robin logic */
  val router = context.actorOf(RoundRobinPool(5).props(Props[Worker]), "router")

  def receive = LoggingReceive {
    case Start => router ! Broadcast(HeadsUp) // should reach *all* workers
    case Work => router ! Work4You // should reach *one* worker (round robin)
  }

}

object Worker {
  case object HeadsUp
  case object Work4You
}

class Worker extends Actor with ActorLogging {
  def receive = LoggingReceive {
    case HeadsUp => log.debug("OK, I am ready... Give me work, " + sender()) // sender equals *master* (not router)
    case Work4You => log.debug("Alright, I am on it!")
  }
}