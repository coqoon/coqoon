/* BuildManager.scala
 * Support for running incremental build loops
 * Copyright © 2013 Alexander Faithfull
 * 
 * You may use, copy, modify and/or redistribute this code subject to the terms
 * of either the license of Kopitiam or the Apache License, version 2.0 */

package dk.itu.sdg.kopitiam

import org.eclipse.core.runtime.CoreException

object BuildManager {
  trait BuildTask {
    import BuildTask._

    /* Called repeatedly by the build loop */
    def build : State
    /* Called when the build loop is done with a task */
    def cleanup : Unit
    
    private var state_ : State = Waiting
    protected[BuildManager] def state = state_
    protected[BuildManager] def state_=(s : State) = (state_ = s)
  }
  object BuildTask {
    sealed abstract class State
    
    /* This task is not ready to be built */
    case object Waiting extends State
    /* This task was ready to be built, but the build failed */
    case class Failed(c : CoreException) extends State
    /* This task removed itself from the build queue for some reason (other
     * than that a build failed) */
    case object Abandoned extends State
    /* This task was built successfully */
    case object Succeeded extends State
  }
  
  def buildLoop(toBuild_ : Set[BuildTask]) = {
    toBuild_.foreach(_.state = BuildTask.Waiting)
    
    var toBuild = toBuild_
    var continue = true
    do {
      val (done, waiting) = toBuild.partition(a => {
        a.state = a.build
        if (a.state != BuildTask.Waiting) {
          a.cleanup
          true
        } else false
      })
      toBuild = waiting
      if (done.isEmpty)
        continue = false
    } while (continue)
    
    toBuild.foreach(_.cleanup)
  }
}