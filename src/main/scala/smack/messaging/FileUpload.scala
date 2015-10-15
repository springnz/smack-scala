package smack.scala

import java.io.File
import java.net.URI

import scala.concurrent.{ExecutionContext, Future}

case class FileDescription(value: Option[String])

trait FileUpload {
  def upload(file: File, description: FileDescription)(implicit ec: ExecutionContext): Future[URI]
}
