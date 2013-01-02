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
  
  val doesNotExist = optInt("does.not.exist")
  val consoleCols = optInt("console.cols")
}