import org.apache.pekko
import pekko.actor._
import scala.concurrent.duration._
import scala.util.Random
// Przykład wykorzystania Planisty (Scheduler)

object TickActor {
  val Tick = "tick"
}

case class Init()
case class Fire(castel: ActorRef)
case class Shoot()
case class Hit(activeDefenders:Int)




class Defender extends Actor {
 override def receive: Receive = {
  case Fire(castel: ActorRef)=>{
    castel ! Shoot
  }
  case Hit(activeDefenders: Int)=>{
    val hitChance = activeDefenders / 200.0
      if (Random.nextDouble() < hitChance) {
        println("actor is dead")
        context.stop(self)       
      }
  }
 }
}

class Castel(name: String) extends Actor {

  override def receive: Receive ={
    case Init => {
     
      var defenders: Set[ActorRef] = Set((1 to 100).map(_ => context.actorOf(Props[Defender]())): _*)
      context.become(game(defenders))
      defenders.foreach(context.watch)
    }
    
     def game(defenders: Set[ActorRef]): Receive = {
      case Fire(castel) => { 
       defenders.foreach(defender =>
          defender ! Fire(castel)
        )
      }
      case Shoot =>{
        if (defenders.nonEmpty) {
        val victim = defenders.toSeq(Random.nextInt(defenders.size))
        victim ! Hit(defenders.size)
      }
    }
    case Terminated(defender) =>
      println("defender fallen")
      val new_defenders =  defenders - defender
      if (new_defenders.isEmpty) {
        println(s"$name has fallen! The battle is over.")
        context.system.terminate()
      }
      else context.become(game(new_defenders))
  }
  }
}





class TickActor extends Actor {
  import TickActor._
  def receive = {
    case Tick => {
        val castle1 = context.actorOf(Props(new Castel("Castle 1")), "castle1")
        castle1 ! Init
        val castle2 = context.actorOf(Props(new Castel("Castle 2")), "castle2")
        castle2 ! Init
        context.become(game(castle1, castle2))
    }
  }
    def game(castel1: ActorRef, castel2:ActorRef): Receive = {
      case Tick => {
       castel1 ! Fire(castel2)
       castel2 ! Fire(castel1) 
           }
    }
  }



  
@main 
def zad1: Unit = {

  val system = ActorSystem("system")
  import system.dispatcher

  val tickActor = system.actorOf(Props[TickActor](), "defender")

  val ticker = system.scheduler.scheduleWithFixedDelay(
    Duration.Zero,
    999.milliseconds,
    tickActor,
    TickActor.Tick
  )




 //system.terminate()

}
