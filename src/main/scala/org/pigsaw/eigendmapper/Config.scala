package org.pigsaw.eigendmapper

import com.typesafe.config.ConfigFactory
import com.typesafe.config.ConfigException

object Config {
  private val conf = ConfigFactory.load()
  
  private def optInt(key: String): Option[Int] =
    try {
      Some(conf.getInt(key))
    }
    catch {
      case e: ConfigException => None
    }
  
  private def optStringList(key: String): Option[List[String]] =
    try {
      import scala.collection.JavaConverters._
      val list = conf.getStringList(key).asScala.toList
      Some(list)
    }
    catch {
      case e: ConfigException => None
    }
  
  val doesNotExist = optInt("does.not.exist")
  val consoleCols = optInt("console.cols")
  
  val bin = optStringList("bin")
}