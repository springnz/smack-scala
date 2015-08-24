import org.jivesoftware.smack.ConnectionConfiguration.SecurityMode
import org.jivesoftware.smack.chat._
import org.jivesoftware.smack.packet.Message
import org.jivesoftware.smack.roster.Roster
import org.jivesoftware.smack.roster.RosterListener
import org.jivesoftware.smack.tcp.XMPPTCPConnection
import org.jivesoftware.smack.tcp.XMPPTCPConnectionConfiguration
import org.jivesoftware.smackx.muc.MultiUserChatManager
import collection.JavaConversions._

trait Setup {
  val config: XMPPTCPConnectionConfiguration
  val username: String
}

trait LocalSetup extends Setup {
  override val username = "admin4@corp"

  override val config = XMPPTCPConnectionConfiguration.builder
    .setUsernameAndPassword("admin4", "admin4")
    .setServiceName("corp")
    .setHost("akllap015.corp") //localhost
    .setSecurityMode(SecurityMode.disabled)
    .build
}

trait PublicSetup extends Setup {
  override val username = "michaelpollmeier@wtfismyip.com"
  override val config = XMPPTCPConnectionConfiguration.builder
    .setUsernameAndPassword("michaelpollmeier", "adfrtvmfkolffmcwryaaf")
    .setServiceName("wtfismyip.com")
    .setHostnameVerifier(new javax.net.ssl.HostnameVerifier() {
      override def verify(hostname: String, session: javax.net.ssl.SSLSession): Boolean = {
        println(s"verifying $hostname")
        true
      }
    })
    .build
}

object SimpleChat extends App with LocalSetup {
  val conn = new XMPPTCPConnection(config)

  conn.connect().login()

  val chatManager = ChatManager.getInstanceFor(conn)
  chatManager.addChatListener(new ChatManagerListener() {
    override def chatCreated(chat: Chat, createdLocally: Boolean): Unit = {
      println(s"ChatManagerListener: chat created: $chat; locally: $createdLocally")
    }
  })

  def testPM(): Unit = {
    val chat = chatManager.createChat(username)
    chat.addMessageListener(new ChatMessageListener() {
      override def processMessage(chat: Chat, message: Message): Unit = {
        println(s"ChatMessageListener: received message for $chat : $message")
      }
    })
    chat.sendMessage("hello world")
  }

  def testMUC(): Unit = {
    val multiChat = MultiUserChatManager.getInstanceFor(conn)
    multiChat.getServiceNames().foreach { service =>
      println(service)
      println(multiChat.isServiceEnabled(service))
      val muc = multiChat.getMultiUserChat("myroom@corp")
      muc.createOrJoin("testbot")
      println("hosted rooms: " + multiChat.getHostedRooms(service).toList)
      println("---------------")
    }
  }

  def testRoster(): Unit = {
    val roster = Roster.getInstanceFor(conn)
    println("roster loaded: " + roster.isLoaded)
    roster.reload()
    roster.addRosterListener(new RosterListener {
                               def entriesAdded   (entries: java.util.Collection[String]): Unit = { println(entries.toList) }
                               def entriesDeleted (entries: java.util.Collection[String]): Unit = { println(entries.toList) }
                               def entriesUpdated (entries: java.util.Collection[String]): Unit = { println(entries.toList) }
                               def presenceChanged(presence: org.jivesoftware.smack.packet.Presence): Unit = { println(presence) }
                             })

    Thread.sleep(1000)
    // roster.
    println("roster loaded: " + roster.isLoaded)
    println("groups: " + roster.getGroups.toList)
    println("entries: " + roster.getEntries.toList)
    println(roster.getPresence("admin3@corp"))
    println(roster.getPresence("admin3@akllap015.corp"))
    println(roster.getPresence("admin@mp"))
    println(roster.getPresence("admin4@corp"))
  }

  testPM()
  // testMUC()
  // testRoster()

  Thread.sleep(1000)
  conn.disconnect()
  println("done")
}
