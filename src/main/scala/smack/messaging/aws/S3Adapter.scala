package smack.scala.aws

import java.io.File
import awscala._, s3._
import com.typesafe.config.ConfigFactory
import scala.concurrent.ExecutionContext.Implicits.global
import smack.scala.{FileDescription, FileUpload}

import scala.concurrent.Future

class S3Adapter extends FileUpload {
  implicit val s3 = S3()
  lazy val config = ConfigFactory.load()
  lazy val bucketName = config.getString("aws.s3.bucket")

  val bucket: Bucket = s3.createBucket(bucketName)
  override def upload(file:File, description: FileDescription) = Future {
    val putObj = bucket.put(description.value.getOrElse(file.getName), file)
    s3.generatePresignedUrl(bucketName, putObj.key, putObj.expirationTime.toDate).toURI
  }
}
