package smack.scala.aws

import java.io.File
import awscala._, s3._
import com.typesafe.config.ConfigFactory
import smack.scala.{ FileDescription, FileUpload }

import scala.concurrent.{ExecutionContext, Future}

class S3Adapter extends FileUpload {
  implicit val s3 = S3()
  lazy val config = ConfigFactory.load()
  lazy val bucketName = config.getString("aws.s3.bucket")
  lazy val bucket = s3.bucket(bucketName).getOrElse(s3.createBucket(bucketName))
  override def upload(file: File, description: FileDescription)(implicit ec: ExecutionContext) = {
    Future {
      val putObj = bucket.put(description.value.getOrElse(file.getName), file)
      s3.generatePresignedUrl(bucketName, description.value.getOrElse(file.getName), putObj.getExpirationTime).toURI
    }
  }
}
