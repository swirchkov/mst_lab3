package wumpusworld

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import wumpusworld.Navigator.ActionRequest

class Navigator {
  def navigatorActor: Behavior[ActionRequest] = Behaviors.receive((context, message) => {
    val percept = message.wumpusPercept
    val action = Handler.calculateAction(percept)

    message.sender ! Navigator.ActionResponse(action._2, action._1)
    Behaviors.same
  })
}


object Navigator {

  sealed trait Percept

  case class ActionRequest(wumpusPercept: WumpusPercept, message: String, sender: ActorRef[ActionResponse])

  case class ActionResponse(action: SpeleologistAction, look: Look)

  case object Stench extends Percept

  case object Breeze extends Percept

  case object Scream extends Percept

  case object Glitter extends Percept

  case object Bump extends Percept

}