package org.pigsaw.eigendmapper

import com.typesafe.config.ConfigFactory
import com.typesafe.config.ConfigException

object Config {
  private val conf = ConfigFactory.load()
  
  // Get an Int from the config (or throw an exception)

  private def getInt(key: String): Int = conf.getInt(key)
  
  // Get a List[String] from the config (or throw an exception)
  
  private def getStringList(key: String): List[String] = {
      import scala.collection.JavaConverters._
      conf.getStringList(key).asScala.toList
  }
  
  // Get something from the config, as an Option
  
  private def optGet[T](fn: (String) => T, key: String): Option[T] =
    try {
      Some(fn(key))
    }
    catch {
      case e: ConfigException => None
    }
  
  val doesNotExist = optGet(getInt, "does.not.exist")
  val consoleCols = optGet(getInt, "console.cols")
  
  val eigenDBin = optGet(getStringList, "eigend.bin")
}