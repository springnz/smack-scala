package smack.scala

import java.io.File
import java.net.URI

import smack.scala.Client.Messages._
import smack.scala.Client._
import akka.actor.{ ActorSystem, Props }
import akka.pattern.ask
import akka.testkit.{ TestActorRef, TestProbe }
import akka.util.Timeout
import com.typesafe.config.ConfigFactory
import java.util.UUID
import org.jivesoftware.smack.packet.Presence
import org.jivesoftware.smack.roster.Roster
import org.scalatest
import org.scalatest.exceptions.TestFailedException
import org.scalatest.{ BeforeAndAfterEach, Matchers, WordSpec }
import scala.collection.JavaConversions._
import scala.concurrent.{ Future, Await }
import scala.concurrent.duration._
import scala.util.{ Failure, Success, Try }
import akka.actor.Status.{ Failure ⇒ ActorFailure }
import scala.concurrent.ExecutionContext.Implicits.global

// this test depends on a running xmpp server (e.g. ejabberd) configured so that admin users can create unlimited users in your environment!
// see http://docs.ejabberd.im/admin/guide/configuration/#modregister for more details
@tags.RequiresEjabberd
class ClientTest extends WordSpec with Matchers with BeforeAndAfterEach {
  implicit var system: ActorSystem = _
  implicit val timeout = Timeout(5 seconds)

  val config = ConfigFactory.load
  val adminUsername = config.getString("messaging.admin.username")
  val adminPassword = config.getString("messaging.admin.password")
  val domain = config.getString("messaging.domain")

  "A client" when {
    "usernames don't have domains " should {
      "connects to the xmpp server" in new TestFunctions {
        connected
      }

      "allows user registration and deletion" in new TestFunctions {
        registration
      }

      "should reject duplicate registration" in new TestFunctions {
        sameRegistration
      }

      "should reject registration with username same as domain" in new TestFunctions {
        invalidRegistration
      }

      "enables users to chat to each other" in new TestFunctions {
        chat
      }

      "enables async chats (message recipient offline)" in new TestFunctions {
        asyncChat
      }

      "enables XEP-0066 file transfers" in new TestFunctions {
        XEP_0066_FileTransfers
      }

      "enables mock file upload" in new TestFunctions {
        fileUpload
      }

      "handles file upload error mock" in new TestFunctions with UploadError {
        fileUploadWithError
      }

      "handles s3 upload" in new TestFunctions with UploadS3 {
        fileUpload
      }

      "handles s3 upload with error" in new TestFunctions with UploadS3 {
        fileUploadWithError
      }

      "informs event listeners about chat partners becoming available / unavailable" in new TestFunctions {
        availability
      }

      "provides information about who is online and offline (roster)" in new TestFunctions {
        roster
      }

      "message receiver subscribes to sender" in new TestFunctions {
        receiverConnects
      }

      "message delivered acknowledgement" in new TestFunctions {
        deliveryAcknowledged
      }

      "async message delivered acknowledgement" in new TestFunctions {
        asyncDeliveryAck
      }

      "message unack tracking" in new TestFunctions {
        deliveryEnsureIdTracking
      }

      "creates group chatroom" in new TestFunctions {
        createChatRoom
      }

      "fail on chatroom already exists" in new TestFunctionsWithDomain {
        createMultipleChatRooms
      }

      "fail on non-member joining room" in new TestFunctionsWithDomain {
        failNonMember
      }

      "member joining room" in new TestFunctionsWithDomain {
        memberJoin
      }

      "non admin can't accept member" in new TestFunctionsWithDomain {
        nonAdminMemberFail
      }

      "can't join with same nickname" in new TestFunctionsWithDomain {
        sameNickname
      }

      "admin can remove member" in new TestFunctionsWithDomain {
        removeMember
      }

      "non admin can\'t remove member" in new TestFunctionsWithDomain {
        nonAdminRemoveFail
      }
    }

    "usernames have domains " should {
      "connects to the xmpp server" in new TestFunctionsWithDomain {
        connected
      }

      "allows user registration" in new TestFunctionsWithDomain {
        registration
      }

      "should reject duplicate registration" in new TestFunctionsWithDomain {
        sameRegistration
      }

      "should reject registration with username same as domain" in new TestFunctionsWithDomain {
        invalidRegistration
      }

      "enables users to chat to each other" in new TestFunctionsWithDomain {
        chat
      }

      "enables async chats (message recipient offline)" in new TestFunctionsWithDomain {
        asyncChat
      }

      "enables XEP-0066 file transfers" in new TestFunctionsWithDomain {
        XEP_0066_FileTransfers
      }

      "enables file upload mock" in new TestFunctionsWithDomain {
        fileUpload
      }

      "handles file upload error mock" in new TestFunctionsWithDomain with UploadError {
        fileUploadWithError
      }

      "handles s3 upload" in new TestFunctionsWithDomain with UploadS3 {
        fileUpload
      }

      "handles s3 upload with error" in new TestFunctionsWithDomain with UploadS3 {
        fileUploadWithError
      }

      "informs event listeners about chat partners becoming available / unavailable" in new TestFunctionsWithDomain {
        availability
      }

      "provides information about who is online and offline (roster)" in new TestFunctionsWithDomain {
        roster
      }

      "message receiver subscribes to sender" in new TestFunctionsWithDomain {
        receiverConnects
      }

      "message delivered acknowledgement" in new TestFunctionsWithDomain {
        deliveryAcknowledged
      }

      "async message delivered acknowledgement" in new TestFunctionsWithDomain {
        asyncDeliveryAck
      }

      "message unack tracking" in new TestFunctionsWithDomain {
        deliveryEnsureIdTracking
      }

      "message history" in new TestFunctionsWithDomain {
        chatHistory
      }

      "creates group chatroom" in new TestFunctionsWithDomain {
        createChatRoom
      }

      "fail on chatroom already exists" in new TestFunctionsWithDomain {
        createMultipleChatRooms
      }

      "fail on non-member joining room" in new TestFunctionsWithDomain {
        failNonMember
      }

      "member joining room" in new TestFunctionsWithDomain {
        memberJoin
      }

      "non admin can't accept member" in new TestFunctionsWithDomain {
        nonAdminMemberFail
      }

      "can't join with same nickname" in new TestFunctionsWithDomain {
        sameNickname
      }

      "admin can remove member" in new TestFunctionsWithDomain {
        removeMember
      }

      "non admin can\'t remove member" in new TestFunctionsWithDomain {
        nonAdminRemoveFail
      }
    }
  }

  class TestFunctions extends AnyRef with Fixture {
    def connected: Unit = {
      val connected = adminUser ? Connect(User(adminUsername), Password(adminPassword))
      connected.value.get shouldBe Success(Messages.Connected)
      adminUser ! Disconnect
    }

    def registration: Unit = {
      val username = randomUsername
      val userPass = Password(username.value)

      val connected = adminUser ? Connect(User(adminUsername), Password(adminPassword))
      adminUser ! RegisterUser(username, userPass)

      val connected1 = user1 ? Connect(username, userPass)
      connected1.value.get shouldBe Success(Messages.Connected)

      user1 ! DeleteUser
      val connected2 = user1 ? Connect(username, userPass)
      connected2.value.get.get match {
        case ConnectError(t) ⇒ //that's all we want to check
      }
    }

    def sameRegistration: Unit = {
      val username = randomUsername
      val userPass = Password(username.value)

      val connected = adminUser ? Connect(User(adminUsername), Password(adminPassword))
      adminUser ! RegisterUser(username, userPass)
      val registration = adminUser ? RegisterUser(username, userPass)
      registration.value.get shouldBe Failure(DuplicateUser(username))
    }

    def invalidRegistration: Unit = {
      val username = User(domain);
      val userPass = Password(username.value)

      val connected = adminUser ? Connect(User(adminUsername), Password(adminPassword))
      val registration = adminUser ? RegisterUser(username, userPass)
      registration.value.get shouldBe Failure(InvalidUserName(username))
    }

    def chat: Unit = {
      withTwoConnectedUsers {
        user1 ! SendMessage(username2, testMessage)
        verifyMessageArrived(user2Listener, username1, username2, testMessage)
      }
    }

    def asyncChat: Unit = {
      withTwoUsers {
        user1 ! Connect(username1, user1Pass)
        val user2Listener = newEventListener
        user2 ! RegisterEventListener(user2Listener.ref)

        user1 ! SendMessage(username2, testMessage)

        // yeah, sleeping is bad, but I dunno how else to make this guaranteed async.
        Thread.sleep(1000)
        user2 ! Connect(username2, user2Pass)

        verifyMessageArrived(user2Listener, username1, username2, testMessage)
      }
    }

    def XEP_0066_FileTransfers = {
      withTwoConnectedUsers {
        val fileUrl = URI.create("https://raw.githubusercontent.com/mpollmeier/gremlin-scala/master/README.md")
        val fileDescription = FileDescription(Some("file description"))
        user1 ! SendUrlMessage(username2, fileUrl, fileDescription)

        user2Listener.expectMsgPF(3 seconds, "xep-0066 file transfer") {
          case FileMessageReceived(chat, message, outOfBandData) ⇒
            chat.getParticipant should startWith(username1.value)
            message.getTo should startWith(username2.value)
            outOfBandData.url shouldBe fileUrl
            outOfBandData.desc shouldBe fileDescription
        }
      }
    }

    def fileUpload = {
      withTwoConnectedUsers {
        val file = new File("./src/test/resources/shuttle.jpg")
        val fileDescription = FileDescription(Some("shuttle.jpg"))
        user1 ! SendFileMessage(username2, file, fileDescription)

        var fileUri = URI.create("") //how to do this less imperatively?
        user1Listener.fishForMessage(30 seconds, "file uploaded") {
          case FileUploaded(user, uri, description) ⇒
            user.value should startWith(username1.value)
            description shouldBe fileDescription
            fileUri = uri
            true
          case ActorFailure(FileUploadError(ex)) ⇒
            throw new Exception(ex)
            false
          case _ ⇒ false
        }

        user2Listener.fishForMessage(3 seconds, "file transfer") {
          case FileMessageReceived(chat, message, outOfBandData) ⇒
            chat.getParticipant should startWith(username1.value)
            message.getTo should startWith(username2.value)
            outOfBandData.url shouldBe fileUri
            outOfBandData.desc shouldBe fileDescription
            true
          case _ ⇒ false
        }
      }
    }

    def fileUploadWithError = {
      withTwoConnectedUsers {
        val file = new File("erroredimage")
        val fileDescription = Some("file description")
        user1 ! SendFileMessage(username2, file, FileDescription(fileDescription))

        user1Listener.fishForMessage(3 seconds, "file upload error") {
          case ActorFailure(FileUploadError(ex)) ⇒
            true
          case _ ⇒ false
        }
      }
    }

    def availability = {
      withTwoConnectedUsers {
        user1 ! SendMessage(username2, testMessage)
        verifyMessageArrived(user2Listener, username1, username2, testMessage)
        user1Listener.fishForMessage(3 seconds, "notification that user2 came online") {
          case UserBecameAvailable(user) ⇒
            user.value should startWith(username2.value)
            true
          case _ ⇒ false
        }

        user2 ! Disconnect
        user1Listener.fishForMessage(3 seconds, "notification that user2 went offline") {
          case UserBecameUnavailable(user) ⇒
            user.value should startWith(username2.value)
            true
          case _ ⇒ false
        }

        user2 ! Connect(username2, user2Pass)
        user1Listener.fishForMessage(3 seconds, "notification that user2 came online") {
          case UserBecameAvailable(user) ⇒
            user.value should startWith(username2.value)
            true
          case _ ⇒ false
        }
      }
    }

    def deliveryAcknowledged = {
      withTwoConnectedUsers {
        val user1MessageId = user1 ? SendMessage(username2, testMessage)
        verifyMessageArrived(user2Listener, username1, username2, testMessage)
        verifyMessageDelivery(user1Listener, username1, username2, user1MessageId)
      }
    }

    def asyncDeliveryAck: Unit = {
      withTwoUsers {
        user1 ! Connect(username1, user1Pass)
        val user2Listener = newEventListener
        user2 ! RegisterEventListener(user2Listener.ref)

        val user1MessageId = user1 ? SendMessage(username2, testMessage)

        // yeah, sleeping is bad, but I dunno how else to make this guaranteed async.
        Thread.sleep(1000)
        user2 ! Connect(username2, user2Pass)

        verifyMessageArrived(user2Listener, username1, username2, testMessage)
        verifyMessageDelivery(user1Listener, username1, username2, user1MessageId)
      }
    }

    def deliveryEnsureIdTracking = {
      withTwoUsers {
        user1 ! Connect(username1, user1Pass)
        val user2Listener = newEventListener
        user2 ! RegisterEventListener(user2Listener.ref)

        val user1MessageIdFuture = (user1 ? SendMessage(username2, testMessage)).mapTo[MessageId]
        val user1MessageId = Await.result(user1MessageIdFuture, 3 seconds)

        val unackedMessageFuture = (user1 ? GetUnackMessages(username2)).mapTo[GetUnackMessagesResponse]
        Await.result(unackedMessageFuture, 3 seconds) match {
          case GetUnackMessagesResponse(user, ids) ⇒
            user.value should startWith(username2.value)
            ids.size shouldBe 1
            ids(0).id shouldBe user1MessageId
        }

        // yeah, sleeping is bad, but I dunno how else to make this guaranteed async.
        Thread.sleep(1000)
        user2 ! Connect(username2, user2Pass)

        verifyMessageArrived(user2Listener, username1, username2, testMessage)
        verifyMessageDelivery(user1Listener, username1, username2, user1MessageIdFuture)
        val emptyUnacked = (user1 ? GetUnackMessages(username2)).mapTo[GetUnackMessagesResponse]
        Await.result(emptyUnacked, 3 seconds) match {
          case GetUnackMessagesResponse(user, ids) ⇒
            user.value should startWith(username2.value)
            ids.isEmpty shouldBe true
        }
      }
    }

    def roster = {
      withTwoConnectedUsers {
        user1 ! SendMessage(username2, testMessage)
        verifyMessageArrived(user2Listener, username1, username2, testMessage)

        user1Listener.fishForMessage(3 seconds, "notification that user2 is in roster") {
          case UserBecameAvailable(user) ⇒
            val roster = getRoster(user1)
            roster.getEntries should have size 1
            val entry = roster.getEntries.head
            entry.getUser should startWith(username2.value)
            roster.getPresence(entry.getUser).getType shouldBe Presence.Type.available
            true
          case _ ⇒ false
        }

        user2 ! Disconnect
        user1Listener.fishForMessage(3 seconds, "notification that user2 is not in roster") {
          case UserBecameUnavailable(user) ⇒
            val roster = getRoster(user1)
            roster.getEntries should have size 1
            val entry = roster.getEntries.head
            entry.getUser should startWith(username2.value)
            roster.getPresence(entry.getUser).getType shouldBe Presence.Type.unavailable
            true
          case _ ⇒ false
        }
      }
    }

    def receiverConnects = {
      withTwoConnectedUsers {
        user1 ! SendMessage(username2, testMessage)
        verifyMessageArrived(user2Listener, username1, username2, testMessage)

        user2Listener.fishForMessage(3 seconds, "notification that user1 is in roster") {
          case UserBecameAvailable(user) ⇒
            val roster = getRoster(user2)
            roster.getEntries should have size 1
            val entry = roster.getEntries.head
            entry.getUser should startWith(username1.value)
            roster.getPresence(entry.getUser).getType shouldBe Presence.Type.available
            true
        }
      }
    }

    def chatHistory = {
      withTwoConnectedUsers {
        user1 ! SendMessage(username2, testMessage)

        val anotherMessage = "another Message"
        user1 ! SendMessage(username2, anotherMessage)
        verifyMessageArrived(user2Listener, username1, username2, testMessage)

        Thread.sleep(1000) //let messages reach archive
        user1 ! ArchiveMessageRequest(username2)
        user1Listener.fishForMessage(3 seconds, "notification that user1 sent a message to user2") {
          case ArchiveMessageResponse(to, from, msg, origStamp, id, _) ⇒
            to.value shouldBe username2.value
            from.value shouldBe username1.value + "/Smack"
            msg.getBody shouldBe testMessage
            true
          case _ ⇒ false
        }

        user1Listener.fishForMessage(3 seconds, "notification that user1 sent a message to user2") {
          case ArchiveMessageResponse(to, from, msg, origStamp, id, _) ⇒
            to.value shouldBe username2.value
            from.value shouldBe username1.value + "/Smack"
            msg.getBody shouldBe anotherMessage
            true
          case _ ⇒ false
        }

        user1Listener.fishForMessage(3 seconds, "notification that user1 is sent end message") {
          case ArchiveMessageEnd(_, _, _, _, _) ⇒ true
          case m                                ⇒ false
        }

      }
    }

    def createChatRoom = {
      withChatRoomsActivated {
        val room = adminUser ? CreateChatRoom(chatRoom)
        room.value.get shouldBe Success(Created)
        val rooms = adminUser ? GetChatRooms
        assert(rooms.value.get.get.asInstanceOf[GetChatRoomsResponse].rooms.contains(fullChatRoom))
      }
    }

    def createMultipleChatRooms = {
      withChatRoomsActivated {
        val room = adminUser ? CreateChatRoom(chatRoom)
        room.value.get shouldBe Success(Created)
        val aroom = adminUser ? CreateChatRoom(chatRoom)
        aroom.value.get shouldBe Failure(RoomAlreadyExists(chatRoom))
      }
    }

    def failNonMember = {
      withChatRoom {
        withTwoConnectedUsers {
          val joined = user1 ? ChatRoomJoin(chatRoom, ChatNickname(username1Nickname))
          joined.value.get shouldBe Failure(Forbidden)
        }
      }
    }

    def memberJoin = {
      withChatRoom {
        withTwoConnectedUsers {
          val reg = adminUser ? RegisterChatRoomMembership(chatRoom, username1)
          reg.value.get shouldBe Success(Joined)
          val joined = user1 ? ChatRoomJoin(chatRoom, ChatNickname(username1Nickname))
          joined.value.get shouldBe Success(Joined)
        }
      }
    }

    def nonAdminMemberFail = {
      withChatRoom {
        withTwoConnectedUsers {
          val reg = adminUser ? RegisterChatRoomMembership(chatRoom, username1)
          reg.value.get shouldBe Success(Joined)
          val joined = user1 ? ChatRoomJoin(chatRoom, ChatNickname(username1Nickname))
          joined.value.get shouldBe Success(Joined)
          val reg2 = user1 ? RegisterChatRoomMembership(chatRoom, username2)
          reg2.value.get shouldBe Failure(Forbidden)
        }
      }
    }

    def sameNickname = {
      withChatRoom {
        withTwoConnectedUsers {
          val reg = adminUser ? RegisterChatRoomMembership(chatRoom, username1)
          reg.value.get shouldBe Success(Joined)
          val joined = user1 ? ChatRoomJoin(chatRoom, ChatNickname(username1Nickname))
          joined.value.get shouldBe Success(Joined)
          val reg2 = adminUser ? RegisterChatRoomMembership(chatRoom, username2)
          reg2.value.get shouldBe Success(Joined)
          val joined2 = user2 ? ChatRoomJoin(chatRoom, ChatNickname(username1Nickname))
          joined2.value.get shouldBe Failure(NicknameTaken(ChatNickname(username1Nickname)))
        }
      }
    }

    def removeMember = {
      withChatRoom {
        withTwoConnectedUsers {
          val reg = adminUser ? RegisterChatRoomMembership(chatRoom, username1)
          reg.value.get shouldBe Success(Joined)
          val joined = user1 ? ChatRoomJoin(chatRoom, ChatNickname(username1Nickname))
          joined.value.get shouldBe Success(Joined)
          val remove = adminUser ? RemoveChatRoomMembership(chatRoom, username1)
          remove.value.get shouldBe Success(Destroyed)
          val joined2 = user1 ? ChatRoomJoin(chatRoom, ChatNickname(username1Nickname))
          joined2.value.get shouldBe Failure(Forbidden)
        }
      }
    }

    def nonAdminRemoveFail = {
      withChatRoom {
        withTwoConnectedUsers {
          val reg = adminUser ? RegisterChatRoomMembership(chatRoom, username1)
          reg.value.get shouldBe Success(Joined)
          val joined = user1 ? ChatRoomJoin(chatRoom, ChatNickname(username1Nickname))
          joined.value.get shouldBe Success(Joined)
          val remove = user1 ? RemoveChatRoomMembership(chatRoom, username1)
          remove.value.get shouldBe Failure(Forbidden)
        }
      }
    }

    private def getRoster(u: TestActorRef[Nothing]): Roster = {
      val rosterFuture = (u ? GetRoster).mapTo[GetRosterResponse]
      Await.result(rosterFuture, 3 seconds).roster
    }
  }

  class TestFunctionsWithDomain extends TestFunctions with FixtureWithDomain {
    assert(username1.value.contains("@"))
    assert(username2.value.contains("@"))
  }

  trait SharedFixture {
    def clientCreator = Props(new Client { override lazy val uploadAdapter = UploadMock })
    lazy val adminUser = TestActorRef(clientCreator)
    lazy val user1 = TestActorRef(clientCreator)
    lazy val user2 = TestActorRef(clientCreator)
  }

  trait Fixture extends SharedFixture {

    val username1 = randomUsername
    val username2 = randomUsername
    val username1Nickname = "user1Nick"
    val username2Nickname = "user2Nick"
    val user1Pass = Password(username1.value)
    val user2Pass = Password(username2.value)
    val user1Listener = newEventListener
    val user2Listener = newEventListener
    val chatRoom = ChatRoom("testroom")
    val chatService = ChatService(config.getString("messaging.service"))
    val fullChatRoom = ChatRoomId(chatRoom, chatService)

    val testMessage = "unique test message" + UUID.randomUUID

    def newEventListener: TestProbe = {
      val eventListener = TestProbe()
      eventListener.ignoreMsg {
        case MessageReceived(_, message) ⇒ message.getSubject == "Welcome!"
      }
      eventListener
    }

    def withTwoUsers(block: ⇒ Unit): Unit = {
      adminUser ! Connect(User(adminUsername), Password(adminPassword))
      adminUser ! RegisterUser(username1, Password(username1.value))
      adminUser ! RegisterUser(username2, Password(username2.value))

      user1 ! RegisterEventListener(user1Listener.ref)
      user2 ! RegisterEventListener(user2Listener.ref)
      try {
        block
      } finally {
        user1 ! DeleteUser
        user2 ! DeleteUser
      }
    }

    def withTwoConnectedUsers(block: ⇒ Unit): Unit =
      withTwoUsers {
        user1 ! Connect(username1, user1Pass)
        user2 ! Connect(username2, user2Pass)
        block
      }

    def withChatRoomsActivated(block: ⇒ Unit): Unit = {
      val connected = adminUser ? Connect(User(adminUsername), Password(adminPassword))
      connected.value.get shouldBe Success(Messages.Connected)
      val destroy = adminUser ? DeleteChatRoom(chatRoom)
      destroy.value.get shouldBe Success(Messages.Destroyed)
      try {
        block
      } finally {
        adminUser ! Disconnect
      }
    }

    def withChatRoom(block: ⇒ Unit): Unit = {
      withChatRoomsActivated {
        try {
          val room = adminUser ? CreateChatRoom(chatRoom)
          room.value.get shouldBe Success(Created)
          block
        } finally {
          adminUser ? DeleteChatRoom(chatRoom)
        }
      }
    }

    def verifyMessageArrived(testProbe: TestProbe, sender: User, recipient: User, messageBody: String): Unit = {
      testProbe.fishForMessage(3 seconds, "expected message to be delivered") {
        case MessageReceived(chat, message) ⇒
          chat.getParticipant should startWith(sender.value)
          message.getTo should startWith(recipient.value)
          message.getBody shouldBe messageBody
          true
        case _ ⇒ false
      }
    }

    def verifyMessageDelivery(testProbe: TestProbe, sender: User, recipient: User, messageIdFuture: Future[Any]): Unit = {
      testProbe.fishForMessage(3 seconds, "notification that message has been delivered") {
        case MessageDelivered(to, msgId) ⇒
          Await.result(messageIdFuture, 3 seconds) match {
            case MessageId(messageId) ⇒
              to.value should startWith(recipient.value)
              msgId.value should equal(messageId)
          }
          true
        case _ ⇒ false
      }
    }
  }

  trait FixtureWithDomain extends Fixture {
    override val username1 = nameWithDomain(randomUsername)
    override val username2 = nameWithDomain(randomUsername)
    override val user1Pass = Password(username1.value)
    override val user2Pass = Password(username2.value)
  }

  trait UploadS3 extends SharedFixture {
    override def clientCreator = Props[Client]
  }

  trait UploadError extends SharedFixture {
    override def clientCreator = Props(new Client { override lazy val uploadAdapter = UploadErrorMock })
  }

  object UploadMock extends FileUpload {
    var files = Map[FileDescription, File]()
    def upload(file: File, description: FileDescription) = Future {
      files = files + (description -> file)
      file.toURI
    }
  }

  object UploadErrorMock extends FileUpload {
    val uploadErrorMsg = "Upload error"
    def upload(file: File, description: FileDescription) = Future {
      throw new Exception(uploadErrorMsg)
    }
  }

  def randomUsername = User(s"testuser-${UUID.randomUUID.toString.substring(9)}")
  def nameWithDomain(u: User) = u.copy(value = u.value + s"@$domain")

  override def beforeEach() {
    system = ActorSystem()
  }

  override def afterEach() {
    system.shutdown()
  }
}
