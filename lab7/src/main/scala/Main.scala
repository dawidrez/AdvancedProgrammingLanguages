import org.apache.pekko
import pekko.actor.{ActorSystem, Actor, ActorLogging, ActorRef, Props}

case class Pilka(players: List[ActorRef])

case class  Graj(player: ActorRef, player2: List[ActorRef])

class Player extends Actor with ActorLogging {
  def receive: Receive = {
    case Graj(player, players) =>
      player ! Pilka(players)
    case Pilka(player::players) =>
      player ! Pilka(players:+ sender())
      log.info(s"Obijam ${self.path.name} - Odbijam")
  }
}

@main
def mainProg: Unit = {
  val system = ActorSystem("PingPong")
  val wieslaw = system.actorOf(Props[Player](), "wieslaw")
  val lukasz = system.actorOf(Props[Player](), "lukasz")
   val dawidek = system.actorOf(Props[Player](), "dawidek")
   val julka = system.actorOf(Props[Player](), "julka")
  val natalka = system.actorOf(Props[Player](), "natalka")
   val genia = system.actorOf(Props[Player](), "genia")
  lukasz ! Graj(wieslaw, List(dawidek,julka,natalka,genia))
}

@main

def mainProg2: Unit = {
  val system = ActorSystem("work_1")
  val wieslaw = system.actorOf(Props[Nadzorca](), "wieslaw")
  wieslaw ! Init(5)
  wieslaw ! Zlecenie(
    List(
      "To jest test",
      "To jest kolejny wiersz",
      "Scala jest fajna",
      "Aktorzy w Scali są potężni",
      "Ulica i ulica to to samo"
    )
  )
  
}

case class Init(liczbaPracownikow: Int)
case class Zlecenie(tekst: List[String])
case class Wykonaj(tekst: String)
case class Wynik(number: Int)


class Nadzorca extends Actor with ActorLogging {
  def receive: Receive = {
    case Init(liczbaPracownikow: Int) =>{
      val system = ActorSystem("work")
       val workers = (1 to liczbaPracownikow).map { i =>
        context.actorOf(Props[Pracownik](), s"worker-$i")
      }.toList
      context.become(wait(workers))
    }
  }
    def wait(employers: List[ActorRef]): Receive = {
      case Zlecenie(tekst: List[String]) => {
       val iterator = tekst.iterator
       employers.foreach(worker =>
        if (iterator.hasNext) {
          worker ! Wykonaj(iterator.next())
        })
        context.become(supervise(iterator, employers, 0, employers.size))
      }
    }
  def supervise(remainingText: Iterator[String],
      workers: List[ActorRef],
      accumulatedResult: Int,
      activeWorkers: Int): Receive={
    case Wynik(partialResult: Int) =>{
     val newAccumulatedResult = accumulatedResult + partialResult
      if (remainingText.hasNext) {
        sender() ! Wykonaj(remainingText.next())
      } else {
        val newActiveWorkers = activeWorkers - 1
        if (newActiveWorkers == 0) {
          log.info(s"Całkowity wynik: $newAccumulatedResult")
          context.become(wait(workers))
        } else {
          context.become(supervise(remainingText, workers, newAccumulatedResult, newActiveWorkers))
        }
    }


  } 

}

}

class Pracownik extends Actor with ActorLogging {
 override def receive: Receive = {
    case Wykonaj(tekst) =>
      val uniqueWords = tekst.split("\\s+").map(_.toLowerCase).toSet.size
      log.info(s"Pracownik ${self.path.name} przetworzył tekst: $tekst - liczba słów: $uniqueWords")
      sender() ! Wynik(uniqueWords)
  }
}