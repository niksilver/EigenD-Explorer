/*
 *  Copyright 2012, 2013 Nik Silver.
 *  
 *  This file is part of EigenD Explorer.
 *
 *  EigenD Explorer is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  EigenD Explorer is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with EigenD Explorer.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.pigsaw.eigendexplorer

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