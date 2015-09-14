package smack.scala

import akka.actor.{ ActorSystem, Props }
import org.jivesoftware.smack.roster.Roster
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
  implicit val timeout = Timeout(3 seconds)

  computerSays("Welcome, sir! Please help me with the following:")
  computerSays("username: "); val username = io.StdIn.readLine
  computerSays("password: "); val password = io.StdIn.readLine
  val connectPromise = chattie ? Connect(User(username), Password(password))
  Await.ready(connectPromise, timeout.duration)
  computerSays("You are connected, sir! Say `help` for usage.")

  var on = true
  while (on) {
    io.StdIn.readLine match {
      case "help" ⇒
        computerSays("Here's what I can do for you, sir.")
        computerSays("`message`  #send a message to some user")
        computerSays("`file`     #send a file to another user (XEP-0066 out of band transfer)")
        computerSays("`roster`   #get information about who is online at the moment")
        computerSays("`exit`     #get me out of here")

      case "roster" ⇒
        val rosterFuture = (chattie ? GetRoster).mapTo[GetRosterResponse]
        val roster = Await.result(rosterFuture, 3 seconds).roster
        roster.getEntries foreach { entry ⇒
          computerSays(s"presence: ${roster.getPresence(entry.getUser)}")
          computerSays(s"user: ${entry.getUser}")
        }

      case "message" ⇒
        computerSays("Who do you want to send a message to, sir?")
        val user = User(io.StdIn.readLine)
        computerSays(s"What do you want to say to ${user.value}, sir?")
        val message = io.StdIn.readLine
        chattie ! SendMessage(user, message)

      case "file" ⇒
        computerSays("Who do you want to send a file to, sir?")
        val user = User(io.StdIn.readLine)
        computerSays(s"What is the file url, sir?")
        val fileUrl = io.StdIn.readLine
        chattie ! SendFileMessage(user, fileUrl, description = None)

      case "exit" ⇒
        chattie ! Disconnect
        computerSays("Shutting down")
        system.shutdown()
        on = false

      case _ ⇒ computerSays("¿Qué? No entiendo. Try again, sir! Say `help` for usage.")
    }
  }

  def computerSays(s: String) = println(s">> $s")
}
