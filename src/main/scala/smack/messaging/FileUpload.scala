package smack.scala

import java.io.File
import java.net.URI

import scala.concurrent.Future

case class FileDescription(value: Option[String])

trait FileUpload {
  def upload(file: File, description: FileDescription):Future[URI]
}
