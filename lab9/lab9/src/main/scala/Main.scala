package example_typed

import org.apache.pekko.actor.typed.{ActorRef, Behavior}
import org.apache.pekko.actor.typed.scaladsl.Behaviors
import scala.util.Random

object Competitor {
  sealed trait Cmd
  case class GenerateScores(round: ActorRef[Round.Cmd]) extends Cmd

  def apply(name: String): Behavior[Cmd] = Behaviors.receive { (context, message) =>
    message match {
      case GenerateScores(round) =>
        val random = new Random()
        val scores = (random.nextInt(20) + 1, random.nextInt(20) + 1, random.nextInt(20) + 1)
        context.log.info(s"$name generated scores: $scores")
        round ! Round.CompetitorResult(context.self, scores)
        Behaviors.same
    }
  }
}

object Round {
  sealed trait Cmd
  case class RunRound(competitors: List[ActorRef[Competitor.Cmd]]) extends Cmd
  case class CompetitorResult(competitor: ActorRef[Competitor.Cmd], score: (Int, Int, Int)) extends Cmd
  case object StopRound extends Cmd

  def apply(organiser: ActorRef[Organiser.Cmd]): Behavior[Cmd] = Behaviors.receive { (context, message) =>
    message match {
      case RunRound(competitors) =>
        context.log.info(s"Running a round with ${competitors.size} competitors...")
        competitors.foreach(_ ! Competitor.GenerateScores(context.self))
        Behaviors.same

      case CompetitorResult(competitor, score) =>
        organiser ! Organiser.Result(competitor, score)
        Behaviors.same

      case StopRound =>
        context.log.info("Stopping the round...")
        Behaviors.stopped
    }
  }
}
import scala.concurrent.duration._
object Organiser {
  sealed trait Cmd
  case object StartCompetition extends Cmd
  case class Result(competitor: ActorRef[Competitor.Cmd], score: (Int, Int, Int)) extends Cmd

  def apply(): Behavior[Cmd] = Behaviors.setup { context =>
    val competitors = (1 to 50).map { i =>
      context.spawn(Competitor(s"Competitor_$i"), s"Competitor_$i")
    }.toList

    Behaviors.receiveMessage {
      case StartCompetition =>
        context.log.info("Starting the competition...")
        val roundActor = context.spawn(Round(context.self), "QualificationRound")
        roundActor ! Round.RunRound(competitors)

        // Wait for qualification results
        waitForMessagesQ(competitors, roundActor, Map())
      case _ => Behaviors.same
    }
 
  }

  private def waitForMessagesQ(
      competitors: List[ActorRef[Competitor.Cmd]],
      round: ActorRef[Round.Cmd],
      answers: Map[ActorRef[Competitor.Cmd], (Int, Int, Int)],
  ): Behavior[Cmd] = Behaviors.receive { (context, message) =>
    message match {
      case Result(competitor, score) =>
        val newAnswers = answers + (competitor -> score)
        context.log.info(s"Received result from ${competitor.path.name}: $score")

        if (newAnswers.size == competitors.size) {
          context.log.info("All results received for the round!")

          round ! Round.StopRound

          val topCompetitors = newAnswers
              .toList
             .sortBy {
                case (_, (score1, score2, score3)) => 
                (- (score1 + score2 + score3), -score1, -score2, -score3)
            }
              .take(20)

            context.log.info(s"Top 20 competitors selected: ${topCompetitors.map(_._1).map(_.path.name)}")

            val finalRoundActor = context.spawn(Round(context.self), "FinalRound")
            finalRoundActor ! Round.RunRound(topCompetitors.map(_._1))

            waitForMessagesF(topCompetitors.map(_._1), finalRoundActor, topCompetitors.toMap, 0)
          } 
        else {
          waitForMessagesQ(competitors, round, newAnswers)
        }
       case _ => Behaviors.same
    }
  }
      
  private def waitForMessagesF(
      competitors: List[ActorRef[Competitor.Cmd]],
      round: ActorRef[Round.Cmd],
      answers: Map[ActorRef[Competitor.Cmd], (Int, Int, Int)],
      received_messages: Int
  ): Behavior[Cmd] = Behaviors.receive { (context, message) =>
    message match {
      case Result(competitor, score) =>
      val newAnswerParticipant = answers.get(competitor) match {
        case Some((oldScore1, oldScore2, oldScore3)) =>
          (
            oldScore1 + score._1,
            oldScore2 + score._2,
            oldScore3 + score._3
          )
        case None => score // If no previous score, use the new one directly
      }
       val newAnswers = answers + (competitor -> newAnswerParticipant)
        context.log.info(s"Received result from ${competitor.path.name}: $score")
        val newReceived = received_messages +1
        if (20 == newReceived) {
          val finalResults = newAnswers.toList.sortBy {
              case (_, (score1, score2, score3)) => (-(score1+score2+score3),-score1, -score2, -score3)
            }

            context.log.info("Final competition results:")
            finalResults.zipWithIndex.foldLeft((List.empty[(Int, String, String)], 1)) {
              case ((acc, currentRank), ((competitor, (score1, score2, score3)), index)) =>
              val newRank = if (index > 0 && (score1==finalResults(index-1)(1)(0) && score1==finalResults(index-1)(1)(1) && score1==finalResults(index-1)(1)(2))) currentRank else index + 1
              val positionEntry = (newRank, competitor.path.name, s"$score1-$score2-$score3 = ${score1+score2+score3}")
              (acc :+ positionEntry, newRank)
                }._1.foreach {
                  case (rank, name, scoreDetails) =>
                    context.log.info(s"$rank. $name - $scoreDetails")
}

            context.log.info("Competition ended.")
            Behaviors.stopped
          }
        else 
          waitForMessagesF(competitors, round, newAnswers, newReceived)
      case _ => Behaviors.same
    }
   
  }
}

import org.apache.pekko.actor.typed.ActorSystem

@main
def demo: Unit = {
  val organiser = ActorSystem(Organiser(), "competition")
  organiser ! Organiser.StartCompetition
}
