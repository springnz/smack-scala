package ylabs.messaging

import akka.actor.{ ActorSystem, Props }
import scala.collection.JavaConversions._
import Client.{ User, Password }
import akka.util.Timeout
import concurrent.duration._
import concurrent.Await
import akka.pattern.ask

object ChatApp extends App {
  import Client.Messages._

  val system = ActorSystem()
  val chattie = system.actorOf(Props[Client], "chatClient")

  var on = true
  computerSays("What now?")
  while (on) {
    io.StdIn.readLine match {
      case "connect" ⇒
        println("username: "); val username = io.StdIn.readLine
        println("password: "); val password = io.StdIn.readLine
        implicit val timeout = Timeout(3 seconds)
        val connectPromise = chattie ? Connect(User(username), Password(password))
        Await.ready(connectPromise, timeout.duration)

      case "openchat" ⇒
        computerSays("Who may i connect you with, sir?")
        val user = User(io.StdIn.readLine)
        chattie ! ChatTo(user)

      case "message" ⇒
        computerSays("Who do you want to send a message to, sir?")
        val user = User(io.StdIn.readLine)
        computerSays("What's your message, sir?")
        val message = io.StdIn.readLine
        chattie ! SendMessage(user, message)

      case "leavechat" ⇒
        computerSays("Who may i disconnect you from, sir?")
        val user = User(io.StdIn.readLine)
        chattie ! LeaveChat(user)

      case "disconnect" ⇒
        chattie ! Disconnect

      case "exit" ⇒
        chattie ! Disconnect
        computerSays("Shutting down")
        system.shutdown()
        on = false

      case _ ⇒ computerSays("¿Qué? No entiendo. Try again, sir!")
    }
  }

  def computerSays(s: String) = println(s">> $s")
}
