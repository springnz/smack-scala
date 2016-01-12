package smack.scala

import java.io.{ InputStream, File }
import java.net.URI

import Client._
import org.jivesoftware.smack.filter.StanzaTypeFilter
import org.jivesoftware.smackx.muc.{ MultiUserChat, MultiUserChatManager }
import smack.scala.aws.S3Adapter
import smack.scala.extensions._
import akka.actor.{ ActorRef, FSM }
import com.typesafe.config.ConfigFactory
import java.util.Collection
import org.jivesoftware.smack.ConnectionConfiguration.SecurityMode
import org.jivesoftware.smack.{SmackException, StanzaListener}
import org.jivesoftware.smack.XMPPException.XMPPErrorException
import org.jivesoftware.smack.chat._
import org.jivesoftware.smack.packet._
import org.jivesoftware.smack.provider.ProviderManager
import org.jivesoftware.smack.roster.{ Roster, RosterListener }
import org.jivesoftware.smack.tcp.{ XMPPTCPConnection, XMPPTCPConnectionConfiguration }
import org.jivesoftware.smackx.iqregister.AccountManager
import org.jivesoftware.smackx.receipts.{ DeliveryReceiptManager, ReceiptReceivedListener }
import org.joda.time.DateTime
import scala.collection.JavaConversions._
import scala.util.{ Failure, Success, Try }
import akka.actor.Status.{ Failure ⇒ ActorFailure, Success ⇒ ActorSuccess }

object Client {

  object ConfigKeys {
    val domain = "messaging.domain"
    val host = "messaging.host"
    val service = "messaging.service"
  }

  case class User(value: String) extends AnyVal {
    def splitUserIntoNameAndDomain(defaultDomain: Domain): (UserWithoutDomain, Domain) = {
      val (u, _) = value.span(c ⇒ c != '@')
      (new UserWithoutDomain(u), defaultDomain)
    }

    def getFullyQualifiedUser(defaultDomain: Domain): UserWithDomain = {
      val (user, domain) = splitUserIntoNameAndDomain(defaultDomain)
      user getFullyQualifiedUser domain
    }
  }

  case class UserWithoutDomain(value: String) extends AnyVal {
    def getFullyQualifiedUser(d: Domain): UserWithDomain = {
      UserWithDomain(s"$value@${d.value}")
    }
  }

  case class UserWithDomain(value: String) extends AnyVal

  case class Password(value: String) extends AnyVal
  case class Domain(value: String) extends AnyVal
  case class MessageId(value: String) extends AnyVal

  case class MemberInfo(user: UserWithDomain, room: ChatRoom)
  case class ChatRoom(value: String) extends AnyVal
  case class ChatService(value: String) extends AnyVal
  case class ChatNickname(value: String) extends AnyVal
  case class ChatRoomId(room: ChatRoom, service: ChatService) {
    override def toString = s"${room.value}@${service.value}"
  }
  object ChatRoomId {
    def apply(jid: String): Option[ChatRoomId] = jid.split('@') match {
      case Array(room, service) ⇒ Some(ChatRoomId(ChatRoom(room), ChatService(service)))
      case _                    ⇒ None
    }
  }

  case class ChatRoomStatus(value: String) extends AnyVal

  sealed trait State
  case object Unconnected extends State
  case object Connected extends State

  sealed trait MessageStatus
  case object Acknowledged extends MessageStatus
  case object Unacknowledged extends MessageStatus

  case class MessageState(
    message: String,
    id: MessageId,
    status: MessageStatus)

  sealed trait ChannelState {
    val messages: Seq[MessageState]

    def withMessageStatus(msgId: Client.MessageId, status: MessageStatus): ChannelState

    def withMessage(msg: MessageState): ChannelState

    def close(): Unit

    def sendMessage(msg: Message): Unit
  }

  case class ChatState(
    channel: Chat,
    override val messages: Seq[MessageState]) extends ChannelState {


    override def withMessageStatus(msgId: MessageId, status: MessageStatus): ChannelState = {
      this.copy( messages =
        messages.find(_.id == msgId) match {
          case Some(messageState) ⇒
            messages.filterNot(_ == messageState) :+ messageState.copy(status = Client.Acknowledged)
          case None ⇒
            messages
        }
      )
    }

    override def withMessage(msg: MessageState): ChatState = this.copy(messages = messages :+ msg)

    override def close(): Unit = channel.close()

    override def sendMessage(msg: Message): Unit = channel.sendMessage(msg)

  }

  case class MultiUserChatState(
    channel: MultiUserChat,
    override val messages: Seq[MessageState]) extends ChannelState {

    override def withMessageStatus(msgId: MessageId, status: MessageStatus): ChannelState = {
      this.copy( messages =
        messages.find(_.id == msgId) match {
          case Some(messageState) ⇒
            messages.filterNot(_ == messageState) :+ messageState.copy(status = Client.Acknowledged)
          case None ⇒
            messages
        }
      )
    }

    override def withMessage(msg: MessageState): MultiUserChatState = this.copy(messages = messages :+ msg)

    override def close(): Unit = channel.leave()

    override def sendMessage(msg: Message): Unit = channel.sendMessage(msg)

  }

  case class Context(
    connection: Option[XMPPTCPConnection],
    chats: Map[UserWithDomain, ChannelState],
    eventListeners: Set[ActorRef])

  object Messages {
    case class RegisterUser(user: User, password: Password)
    object DeleteUser
    case class RegisterEventListener(actor: ActorRef)

    case class CreateChatRoom(room: ChatRoom)
    case class RegisterChatRoomMembership(room: ChatRoom, user: User)
    case class RemoveChatRoomMembership(room: ChatRoom, user: User)
    case class ChatRoomJoin(room: ChatRoom, name: ChatNickname)
    case class GetChatRoomsResponse(rooms: Set[ChatRoomId])
    case class DeleteChatRoom(room: ChatRoom, reason: Option[ChatRoomStatus] = None, alternativeRoom: Option[ChatRoom] = None)

    case class GetRoomMembers(room: ChatRoom)
    case class GetRoomMembersResponse(members: Set[MemberInfo])

    object GetChatRooms
    object Created
    object Destroyed
    object Joined

    case class Connect(user: User, password: Password)
    object Connected
    case class ConnectError(t: Throwable)
    object Disconnect

    object GetRoster
    case class GetRosterResponse(roster: Roster)

    case class SendMultiUserMessage(recipient: ChatRoom, message: String)
    case class SendMessage(recipient: User, message: String)
    case class SendUrlMessage(recipient: User, fileUri: URI, description: FileDescription)
    case class SendFileMessage(recipient: User, file: File, description: FileDescription)
    case class SendStreamMessage(recipient: User, stream: InputStream, description: FileDescription)

    case class GetUnackMessages(user: User)
    case class GetUnackMessagesResponse(user: User, ids: Seq[MessageState])

    case class ArchiveMessageRequest(user: User, start: Option[DateTime] = None, end: Option[DateTime] = None, limit: Option[Int] = None, skipToMessage: Option[MessageId] = None)

    sealed trait ListenerEvent
    case class GroupChatMessageReceived(chatRoom: ChatRoom, message: Message) extends ListenerEvent
    case class MessageReceived(chat: Chat, message: Message) extends ListenerEvent
    case class FileMessageReceived(chat: Chat, message: Message, outOfBandData: OutOfBandData) extends ListenerEvent
    case class UserBecameAvailable(user: User) extends ListenerEvent
    case class UserBecameUnavailable(user: User) extends ListenerEvent
    case class GroupChatMessageDelivered(chat: ChatRoom, message: Message) extends ListenerEvent
    case class MessageDelivered(user: User, messageId: MessageId) extends ListenerEvent
    case class FileUploaded(forUser: User, uri: URI, description: FileDescription) extends ListenerEvent
    case class ArchiveMessageResponse(to: User, from: User, message: Message, origStamp: DateTime, id: String, queryId: Option[String] = None) extends ListenerEvent
    case class ArchiveMessageEnd(firstMessageId: Option[MessageId] = None, lastMessageId: Option[MessageId] = None, index: Option[Int] = None, count: Option[Int] = None, complete: Boolean = true) extends ListenerEvent

    sealed trait SmackError extends Throwable with ListenerEvent
    object Forbidden extends SmackError
    case class RoomAlreadyExists(room: ChatRoom) extends SmackError
    case class DuplicateUser(user: User) extends SmackError
    case class NicknameTaken(nickname: ChatNickname) extends SmackError
    case class InvalidUserName(user: User) extends SmackError
    case class GeneralSmackError(reason: Throwable) extends SmackError
    case class FileUploadError(reason: Throwable) extends SmackError
  }
}

class Client extends FSM[State, Context] {
  startWith(Unconnected, Context(connection = None, chats = Map.empty, eventListeners = Set.empty))

  lazy val config = ConfigFactory.load()
  lazy val defaultDomain = Domain(config.getString(ConfigKeys.domain))
  lazy val chatService = ChatService(config.getString(ConfigKeys.service))
  lazy val host = config.getString(ConfigKeys.host)
  lazy val uploadAdapter: FileUpload = new S3Adapter
  lazy val adminUsername = config.getString("messaging.admin.username")

  def withChatRoom(room: ChatRoom, connection: XMPPTCPConnection)(block: MultiUserChat ⇒ Unit) = {
    val id = ChatRoomId(room, chatService)
    val manager = MultiUserChatManager.getInstanceFor(connection).getMultiUserChat(id.toString)
    block(manager)
  }

  when(Unconnected) {
    case Event(c: Messages.Connect, ctx) ⇒
      Try { connect(c.user, c.password) } match {
        case Success(connection) ⇒
          log.info(s"${c.user} successfully connected")
          connection.addSyncStanzaListener(groupChatMessageListener, new StanzaTypeFilter(classOf[Message]))
          sender ! Messages.Connected
          goto(Connected) using ctx.copy(connection = Some(connection))
        case Failure(t) ⇒
          log.error(t, s"unable to connect as ${c.user}")
          sender ! Messages.ConnectError(t)
          stay
      }

    case Event(Messages.RegisterEventListener(actor), ctx) ⇒
      stay using ctx.copy(eventListeners = ctx.eventListeners + actor)

    case Event(msg: Messages.ListenerEvent, ctx) ⇒
      ctx.eventListeners foreach { _ ! msg }
      stay
  }

  onTransition {
    // format: OFF
    case Unconnected -> Connected ⇒
      sender ! Messages.Connected
    // format: ON
  }

  when(Connected) {
    case Event(Messages.Disconnect, ctx) ⇒
      disconnect(ctx)
      goto(Unconnected) using ctx.copy(connection = None, chats = Map.empty)

    case Event(Messages.RegisterEventListener(actor), ctx) ⇒
      stay using ctx.copy(eventListeners = ctx.eventListeners + actor)

    case Event(msg: Messages.ListenerEvent, ctx @ Context(Some(connection), chats, eventListeners)) ⇒
      val newCtx = msg match {
        case Messages.GroupChatMessageReceived(chat, message) ⇒
          if ( log.isDebugEnabled ) {
            log.debug(s"group chat message received ${chat.value}: $message")
          }

          val roomId = UserWithDomain(chat.value)
          val messageId = message.getStanzaId

          ctx.chats.get(roomId) match {
            case Some(chatCtx)
              if chatCtx.messages.exists( m ⇒ m.id.value == messageId && m.status == Client.Unacknowledged) ⇒
                self ! Messages.GroupChatMessageDelivered(chat, message)
            case e: Any ⇒
          }

          ctx

        case Messages.MessageReceived(chat, message) ⇒
          val (user, domain) = User(chat.getParticipant).splitUserIntoNameAndDomain(defaultDomain)
          subscribeToStatus(connection, user, domain)
          ctx

        case Messages.GroupChatMessageDelivered(chat, message) ⇒
          val roomId = UserWithDomain(chat.value + "@" + chatService.value)

          ctx.copy( chats =
            if(!chats.contains(roomId)) chats
            else chats + (roomId → chats(roomId).withMessageStatus(Client.MessageId(message.getStanzaId),
              Client.Acknowledged))
          )

        case Messages.MessageDelivered(recipient, messageId) ⇒
          val fullUser = recipient.getFullyQualifiedUser(defaultDomain)
          val chat = ctx.chats(fullUser)
          val index = chat.messages.indexWhere(m ⇒ m.id == messageId)

          val updatedMsgs = chat.messages.updated(index, chat.messages(index).copy(status = Acknowledged))

          ctx.copy(chats = ctx.chats + (fullUser ->
            (chat match {
              case c: MultiUserChatState ⇒ c.copy(messages = updatedMsgs)
              case c: ChatState ⇒ c.copy(messages = updatedMsgs)
            })
          ))

        case msg: Messages.ListenerEvent ⇒ ctx
      }
      val reportMsg = msg match {
        case e: Messages.SmackError ⇒ ActorFailure(e)
        case e                      ⇒ e
      }
      eventListeners foreach { _ ! reportMsg }
      stay using newCtx

    case Event(Messages.SendMultiUserMessage(roomId, message), ctx @ Context(Some(connection), chats, _)) ⇒
      val messageToSend = new Message(roomId.value, Message.Type.groupchat)
      messageToSend.setBody(message)
      val msgState = MessageState(message, MessageId(messageToSend.getStanzaId), Unacknowledged)
      val originalSender = sender()

      val room = getMultiUserChat(connection, roomId)

      val chatKey = UserWithDomain(roomId.value + "@" + chatService.value)
      val chat = chats.getOrElse(key = chatKey, MultiUserChatState(room, Seq.empty))
      log.info(s"message sent to $roomId")
      originalSender ! MessageId(messageToSend.getStanzaId)
      chat.sendMessage(messageToSend)

      stay using ctx.copy(chats = ctx.chats + (chatKey → chat.withMessage(msgState)))

    case Event(Messages.SendMessage(recipient, message), ctx @ Context(Some(connection), chats, _)) ⇒
      val (user, domain) = recipient.splitUserIntoNameAndDomain(defaultDomain)
      val fullUser = user getFullyQualifiedUser domain
      val chat = chats.getOrElse(key = fullUser, ChatState(createChat(connection, user, domain), Seq.empty))
      val messageToSend = new Message(recipient.value, message)

      log.info(s"message sent to $recipient")
      sender ! MessageId(messageToSend.getStanzaId)

      val msgState = MessageState(message, MessageId(messageToSend.getStanzaId), Unacknowledged)
      chat.sendMessage(messageToSend)
      stay using ctx.copy(chats = ctx.chats + (fullUser → chat.withMessage(msgState)))

    case Event(Messages.SendUrlMessage(recipient, fileUri, description), ctx) ⇒
      val (user, domain) = recipient.splitUserIntoNameAndDomain(defaultDomain)
      val fullUser = user getFullyQualifiedUser domain
      val chat = ctx.chats.getOrElse(key = fullUser, ChatState(createChat(ctx.connection.get, user, domain), Seq.empty))
      val fileInformation = OutOfBandData(fileUri, description)
      val infoText = "This message contains a link to a file, your client needs to " +
        "implement XEP-0066. If you don't see the file, kindly ask the client developer."
      val message = new Message(recipient.value, infoText)
      message.addExtension(fileInformation)
      chat.sendMessage(message)
      log.info(s"file message sent to $recipient")
      sender ! MessageId(message.getStanzaId)
      val msgState = MessageState(message.getBody, MessageId(message.getStanzaId), Unacknowledged)
      stay using ctx.copy(chats = ctx.chats + (fullUser → chat.withMessage(msgState)))

    case Event(Messages.SendFileMessage(recipient, file, description), ctx) ⇒
      implicit val globalEc = scala.concurrent.ExecutionContext.global
      uploadAdapter.upload(file, description) onComplete {
        case Success(uri) ⇒
          self ! Messages.FileUploaded(User(ctx.connection.get.getUser), uri, description)
          self ! Messages.SendUrlMessage(recipient, uri, description)
        case Failure(ex) ⇒
          log.error(ex, s"could not upload file!")
          self ! Messages.FileUploadError(ex)
      }
      stay

    case Event(register: Messages.RegisterUser, Context(Some(connection), chats, _)) ⇒
      log.info(s"trying to register ${register.user}")
      val accountManager = AccountManager.getInstance(connection)
      Try {
        val (username, _) = register.user.splitUserIntoNameAndDomain(defaultDomain)
        if (username.value == defaultDomain.value) throw new Messages.InvalidUserName(register.user)
        if (username.value == adminUsername) throw new Messages.DuplicateUser(register.user)
        accountManager.createAccount(username.value, register.password.value)
      } match {
        case Success(s) ⇒
          log.info(s"${register.user} successfully created")
          sender ! Messages.Created
        case Failure(t) ⇒
          log.error(t, s"could not register ${register.user}!")
          val response: ActorFailure = t match {
            case ex: Messages.InvalidUserName ⇒ ActorFailure(ex)
            case ex: XMPPErrorException ⇒
              if (ex.getXMPPError.getCondition == XMPPError.Condition.conflict && ex.getXMPPError.getType == XMPPError.Type.CANCEL)
                ActorFailure(Messages.DuplicateUser(register.user))
              else ActorFailure(Messages.GeneralSmackError(t))

            case _ ⇒ ActorFailure(t)
          }
          sender ! response
      }
      stay

    case Event(Messages.GetUnackMessages(user), Context(_, chats, _)) ⇒
      val fullUser = user.getFullyQualifiedUser(defaultDomain)
      val chatlist = chats.get(fullUser)
      val unack = if (chatlist.isDefined) chatlist.get.messages.filter(m ⇒ m.status == Unacknowledged) else Seq[MessageState]()
      sender ! Messages.GetUnackMessagesResponse(user, unack)
      stay

    case Event(Messages.ArchiveMessageRequest(user, start, end, limit, skipTo), Context(Some(connection), _, _)) ⇒
      val request = MAMRequest(user.getFullyQualifiedUser(defaultDomain), start, end, limit, skipTo)
      connection.sendIqWithResponseCallback(request, new StanzaListener {
        override def processPacket(stanza: Stanza): Unit = {}
      })
      stay

    case Event(Messages.CreateChatRoom(chatRoom), Context(Some(connection), _, _)) ⇒
      withChatRoom(chatRoom, connection) { chat ⇒
        val response: akka.actor.Status.Status = Try {
          chat.create("admin")
          val form = chat.getConfigurationForm.createAnswerForm
          form.setAnswer("muc#roomconfig_membersonly", true)
          chat.sendConfigurationForm(form)
        } match {
          case Failure(t) ⇒
            log.error(t, s"Failure to create room ${chatRoom.value}")
            t match {
              case ex: XMPPErrorException if ex.getXMPPError.getCondition == XMPPError.Condition.forbidden && ex.getXMPPError.getType == XMPPError.Type.AUTH ⇒ ActorFailure(Messages.Forbidden)
              case ex: IllegalStateException if ex.getMessage == "Creation failed - User already joined the room." ⇒ ActorFailure(Messages.RoomAlreadyExists(chatRoom))
              case ex: SmackException if ex.getMessage == "Creation failed - Missing acknowledge of room creation." ⇒ ActorFailure(Messages.RoomAlreadyExists(chatRoom))
              case _ ⇒ ActorFailure(Messages.GeneralSmackError(t))
            }
          case _ ⇒ ActorSuccess(Messages.Created)
        }
        sender ! response
      }
      stay

    case Event(Messages.RegisterChatRoomMembership(room, jid), Context(Some(connection), _, _)) ⇒
      val (user, domain) = jid.splitUserIntoNameAndDomain(defaultDomain)
      val fullUser = user getFullyQualifiedUser domain
      withChatRoom(room, connection) { chat ⇒
        val response: akka.actor.Status.Status = Try {
          chat.grantMembership(fullUser.value)
        } match {
          case Failure(t) ⇒
            log.error(t, s"Failure to grant membership")
            t match {
              case ex: XMPPErrorException if ex.getXMPPError.getCondition == XMPPError.Condition.not_allowed && ex.getXMPPError.getType == XMPPError.Type.CANCEL ⇒ ActorFailure(Messages.Forbidden)
              case _ ⇒ ActorFailure(Messages.GeneralSmackError(t))
            }
          case _ ⇒ ActorSuccess(Messages.Joined)
        }
        sender ! response
      }
      stay

    case Event(Messages.RemoveChatRoomMembership(room, jid), Context(Some(connection), _, _)) ⇒
      val (user, domain) = jid.splitUserIntoNameAndDomain(defaultDomain)
      val fullUser = user getFullyQualifiedUser domain
      withChatRoom(room, connection) { chat ⇒
        val response: akka.actor.Status.Status = Try {
          chat.revokeMembership(fullUser.value)
        } match {
          case Failure(t) ⇒
            log.error(t, s"Failure to remove membership")
            t match {
              case ex: XMPPErrorException if ex.getXMPPError.getCondition == XMPPError.Condition.not_allowed && ex.getXMPPError.getType == XMPPError.Type.CANCEL ⇒ ActorFailure(Messages.Forbidden)
              case _ ⇒ ActorFailure(Messages.GeneralSmackError(t))
            }
          case _ ⇒ ActorSuccess(Messages.Destroyed)
        }
        sender ! response
      }
      stay

    case Event(Messages.ChatRoomJoin(room, nickname), Context(Some(connection), _, _)) ⇒
      withChatRoom(room, connection) { chat ⇒
        val response: akka.actor.Status.Status = Try {
          chat.join(nickname.value)
        } match {
          case Failure(t) ⇒
            t match {
              case ex: XMPPErrorException ⇒
                if (ex.getXMPPError.getCondition == XMPPError.Condition.registration_required && ex.getXMPPError.getType == XMPPError.Type.AUTH) {
                  log.warning(s"${nickname.value} failure to join room ${room.value} because not registered")
                  ActorFailure(Messages.Forbidden)
                } else if (ex.getXMPPError.getCondition == XMPPError.Condition.conflict && ex.getXMPPError.getType == XMPPError.Type.CANCEL) {
                  log.warning(s"${nickname.value} failure to join room ${room.value} because nickname already taken")
                  ActorFailure(Messages.NicknameTaken(nickname))
                } else {
                  log.error(t, s"Error joining room ${room.value}")
                  ActorFailure(Messages.GeneralSmackError(t))
                }
              case _ ⇒
                log.error(t, s"Error joining room ${room.value}")
                ActorFailure(t)
            }
          case _ ⇒ ActorSuccess(Messages.Joined)
        }
        sender ! response
      }
      stay

    case Event(Messages.GetChatRooms, Context(Some(connection), _, _)) ⇒
      sender ! Messages.GetChatRoomsResponse(MultiUserChatManager.getInstanceFor(connection).getHostedRooms(chatService.value).map(c ⇒ ChatRoomId.apply(c.getJid).get).toSet)
      stay

    case Event(Messages.GetRoomMembers(room), Context(Some(connection), _, _)) ⇒
      withChatRoom(room, connection) { chat ⇒
        val response: akka.actor.Status.Status = Try {
          val members = chat.getMembers map (m ⇒ MemberInfo(UserWithDomain(m.getJid), room))
          sender ! Messages.GetRoomMembersResponse(members.toSet)
        } match {
          case Failure(t) ⇒ t match {
            case ex: XMPPErrorException ⇒
              if (ex.getXMPPError.getCondition == XMPPError.Condition.forbidden && ex.getXMPPError.getType == XMPPError.Type.AUTH)
                ActorFailure(Messages.Forbidden)
              else ActorFailure(Messages.GeneralSmackError(t))
            case _ ⇒ ActorFailure(t)
          }
          case Success(members) ⇒ ActorSuccess(members)
        }
        sender ! response
      }

      stay

    case Event(Messages.DeleteChatRoom(chatRoom, status, altRoom), Context(Some(connection), _, _)) ⇒
      withChatRoom(chatRoom, connection) { chat ⇒
        val altRoomId = altRoom map (a ⇒ ChatRoomId(a, chatService).toString)
        val response: akka.actor.Status.Status = Try {
          chat.destroy(status.getOrElse(ChatRoomStatus("Destroying room")).value, altRoomId.orNull)
        } match {
          case Failure(t) ⇒ t match {
            case ex: XMPPErrorException ⇒
              if (ex.getXMPPError.getCondition == XMPPError.Condition.item_not_found && ex.getXMPPError.getType == XMPPError.Type.CANCEL)
                ActorSuccess(Messages.Destroyed)
              else ActorFailure(Messages.GeneralSmackError(t))

            case _ ⇒ ActorFailure(t)
          }
          case _ ⇒ ActorSuccess(Messages.Destroyed)
        }
        sender ! response
      }
      stay

    case Event(Messages.DeleteUser, ctx @ Context(Some(connection), _, _)) ⇒
      log.info(s"trying to delete user")
      val accountManager = AccountManager.getInstance(connection)
      Try {
        accountManager.deleteAccount()
      } match {
        case Success(s) ⇒ log.info(s"user successfully deleted")
        case Failure(t) ⇒ log.error(t, s"could not delete user!")
      }
      goto(Unconnected) using ctx.copy(connection = None, chats = Map.empty)

    case Event(Messages.GetRoster, Context(Some(connection), chats, _)) ⇒
      val roster = Roster.getInstanceFor(connection)
      sender ! Messages.GetRosterResponse(roster)
      stay
  }

  def connect(user: User, password: Password): XMPPTCPConnection = {
    val (username, domain) = user.splitUserIntoNameAndDomain(defaultDomain)
    val connection = new XMPPTCPConnection(
      XMPPTCPConnectionConfiguration.builder
        .setUsernameAndPassword(username.value, password.value)
        .setServiceName(domain.value)
        .setHost(host)
        .setSecurityMode(SecurityMode.disabled)
        .setSendPresence(true)
        .build)
    connection.connect().login()
    setupChatManager(connection)
    setupRosterListener(connection)
    setupDeliveryReceiptManager(connection)
    setupCustomExtensions(connection)

    connection
  }

  def disconnect(ctx: Context): Unit = {
    ctx.chats.values.foreach(_.close())
    ctx.connection.foreach(_.disconnect())
    log.info("disconnected")
  }

  def setupChatManager(connection: XMPPTCPConnection): ChatManager = {
    val chatManager = ChatManager.getInstanceFor(connection)
    chatManager.addChatListener(new ChatManagerListener {
      override def chatCreated(chat: Chat, createdLocally: Boolean): Unit = {
        log.info(s"ChatManagerListener: chat created: $chat; locally: $createdLocally")
        chat.addMessageListener(chatMessageListener)
      }
    })
    chatManager
  }

  def setupRosterListener(connection: XMPPTCPConnection): Unit =
    Roster.getInstanceFor(connection).addRosterListener(rosterListener)

  def setupDeliveryReceiptManager(connection: XMPPTCPConnection): Unit = {
    val deliveryManager = DeliveryReceiptManager.getInstanceFor(connection)
    deliveryManager.addReceiptReceivedListener(deliveryReceiptListener)
    deliveryManager.autoAddDeliveryReceiptRequests()
    deliveryManager.setAutoReceiptMode(DeliveryReceiptManager.AutoReceiptMode.always)
    log.info(s"delivery manager created")
  }

  def getMultiUserChat(connection: XMPPTCPConnection, recipient: ChatRoom): MultiUserChat = {
    val chatManager = MultiUserChatManager.getInstanceFor(connection)
    val chat = chatManager.getMultiUserChat(recipient.value + "@" + chatService.value)
    log.debug(s"chat with $recipient created")
    chat
  }

  def createChat(connection: XMPPTCPConnection, recipient: UserWithoutDomain, domain: Domain): Chat = {
    val chatManager = ChatManager.getInstanceFor(connection)
    val chat = chatManager.createChat(recipient.getFullyQualifiedUser(domain).value)
    log.debug(s"chat with $recipient created")
    subscribeToStatus(connection, recipient, domain)
    chat
  }

  def subscribeToStatus(connection: XMPPTCPConnection, user: UserWithoutDomain, domain: Domain): Unit = {
    if (user.value != domain.value) {
      val username = user.getFullyQualifiedUser(domain).value
      val roster = Roster.getInstanceFor(connection)
      if (!roster.getEntries.contains(username)) {
        val presence = new Presence(Presence.Type.subscribe)
        presence.setTo(username)
        log.info(s"requesting roster presence permissions for $user")
        connection.sendStanza(presence)
        log.info(s"requested roster presence permissions for $user")
      }
    }
  }

  def setupCustomExtensions(connection: XMPPTCPConnection) = {
    ProviderManager.addExtensionProvider(OutOfBandData.ElementName, OutOfBandData.XmlNamespace, OutOfBandDataProvider)
    ProviderManager.addExtensionProvider(MAMResponse.ElementName, MAMResponse.XmlNamespace, MAMResponseProvider)
    ProviderManager.addExtensionProvider(MAMFin.ElementName, MAMFin.XmlNamespace, MAMFinProvider)
  }

  val extensions = Set[ExtensionInfoProvider](OutOfBandData, MAMResponse, MAMFin)
  def getExtension(message: Message) = {
    extensions map (e ⇒ Option(message.getExtension[ExtensionElement](e.ElementName, e.XmlNamespace))) collect { case Some(e) ⇒ e }
  }

  val groupChatMessageListener = new StanzaListener {
    override def processPacket(packet: Stanza): Unit = {
      packet.asInstanceOf[Message] match {
        case msg if msg.getType == Message.Type.groupchat ⇒
          val index = msg.getFrom.lastIndexOf('/')
          val chatRoomId =
            if (index > 0) ChatRoom(msg.getFrom.substring(0, index))
            else ChatRoom(msg.getFrom)

          self ! Messages.GroupChatMessageReceived(chatRoomId, packet.asInstanceOf[Message])
        case _ ⇒
      }
    }
  }

  val chatMessageListener = new ChatMessageListener {
    override def processMessage(chat: Chat, message: Message): Unit = {
      // pretty shitty of smack to take a type parameter there... all they do is cast it!
      val extensions = getExtension(message)
      if (extensions.isEmpty) {
        log.debug(s"ChatMessageListener: received message for $chat : ${message.toXML}")
        self ! Messages.MessageReceived(chat, message)
      } else {
        extensions foreach {
          case ob: OutOfBandData ⇒ OutOfBandData.fromXml(ob.toXML) match {
            case Success(outOfBandData) ⇒
              log.debug(s"ChatMessageListener: received file message for $chat : $message")
              self ! Messages.FileMessageReceived(chat, message, outOfBandData)
            case Failure(t) ⇒
              log.error(t, "ChatMessageListener: received file message but was unable to parse the extension into a XEP-0066 format")
              self ! Messages.MessageReceived(chat, message)
          }

          case MAMResponse(msg, origStamp, id, queryId)                      ⇒ self ! Messages.ArchiveMessageResponse(User(msg.getTo), User(msg.getFrom), msg, origStamp, id, queryId)
          case MAMFin(firstMessageId, lastMessageId, index, count, complete) ⇒ self ! Messages.ArchiveMessageEnd(firstMessageId, lastMessageId, index, count, complete)
        }
      }
    }
  }

  val rosterListener = new RosterListener {
    def entriesAdded(entries: Collection[String]): Unit = {
      log.debug("roster entries added: " + entries.toList)
    }

    def entriesDeleted(entries: Collection[String]): Unit = {
      log.debug("roster entries deleted: " + entries.toList)
    }

    def entriesUpdated(entries: Collection[String]): Unit = {
      log.debug("roster entries updated: " + entries.toList)
    }

    def presenceChanged(presence: Presence): Unit = {
      val user = User(presence.getFrom)
      presence.getType match {
        case Presence.Type.available ⇒
          log.debug(s"$user became available")
          self ! Messages.UserBecameAvailable(user)
        case Presence.Type.unavailable ⇒
          log.debug(s"$user became unavailable")
          self ! Messages.UserBecameUnavailable(user)
        case _ ⇒ log.debug(s"presence changed: $presence")
      }
    }
  }

  val deliveryReceiptListener = new ReceiptReceivedListener {
    override def onReceiptReceived(from: String, to: String, receiptId: String, stanza: Stanza): Unit = {
      log.debug(s"receipt received $from sent to $to with id $receiptId")
      val index = from.lastIndexOf('/')
      val user = if (index > 0) User(from.substring(0, index)) else User(from)
      self ! Messages.MessageDelivered(user, MessageId(receiptId))
    }
  }

  initialize()
}

