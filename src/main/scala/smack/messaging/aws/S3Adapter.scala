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
  //lazy val bucket = s3.bucket(bucketName).get
  override def upload(file:File, description: FileDescription) = Future {
    s3.buckets map (b => println( b.name))
    println("GOING TO PUT")
    val b = s3.createBucket(bucketName)
    println("FOUND BUCKET")
    b.put(description.value.getOrElse(file.getName), file)
    println("PUTTED3")
    val putObj = s3.putObject(bucketName, description.value.getOrElse(file.getName), file)
    println("PUTTED")
    s3.generatePresignedUrl(bucketName, description.value.getOrElse(file.getName), putObj.getExpirationTime).toURI
  }
}
