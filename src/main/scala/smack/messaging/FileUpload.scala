package smack.scala

import java.io.File

case class FileDescription(value: Option[String])

trait FileUpload {
  def upload(file: File, description: FileDescription)
}
