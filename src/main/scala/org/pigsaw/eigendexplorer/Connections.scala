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

import Preamble._

/**
 * A connection between two ports, the master and the slave.
 * Both ports should be of the form `<agent1>#11.2.33`.
 * @param master  The port the connection is from.
 * @param slave  The port the connection is to.
 */
case class Connection(val master: PortID, val slave: PortID) {
  /**
   * Generate an unqualified version of this connection. I.e. If the id of either
   * the master or the slave is of the form &lt;<main:agentnameN&gt; then it's
   * converted to &lt;agentnameN&gt;.
   */
  def unqualified: Connection = {
    val unqualMaster = master.unqualified
    val unqualSlave = slave.unqualified
    if ((unqualMaster eq master) && (unqualSlave eq slave)) this
    else Connection(unqualMaster, unqualSlave)
  }
  
  /**
   * Default the qualifier, in both the master and the slave, to be the given
   * pos if there is no explicit qualifier already
   */
  def defaultQualifier(p: Pos) =
    if (master.qualifier != "" && slave.qualifier != "")
      this
    else
      Connection(master.defaultQualifier(p), slave.defaultQualifier(p))

  /**
   * Get the agent names embedded in the master and slave port ids.
   */
  def agents: Set[Agent] = Set() + master.agent + slave.agent
  
  /**
   * See if one port in this connection has the given position
   */
  def hasPos(p: Pos): Boolean =
    (master hasPos p) || (slave hasPos p)
}
