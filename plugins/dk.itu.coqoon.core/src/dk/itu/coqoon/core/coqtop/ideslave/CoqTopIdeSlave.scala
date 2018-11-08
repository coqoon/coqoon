/* CoqTopIdeSlave.scala
 * A wrapper around coqtop's -ideslave protocol
 * Copyright © 2013 Alexander Faithfull
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy
 * of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations
 * under the License. */

package dk.itu.coqoon.core.coqtop.ideslave

trait CoqTopIdeSlave {
  def version : String

  def kill
  def interrupt
}

trait CoqTopIdeSlave_v20120710 extends CoqTopIdeSlave {
  import CoqTypes._

  override def version = "20120710"

  /* All of these methods perform synchronised and blocking I/O */
  def interp(raw : raw, verbose : verbose, string : String) : value[String]
  def rewind(steps : Int) : value[Int]
  def goals : value[Option[goals]]
  def hints : value[Option[(List[hint], hint)]]
  def status : value[status]
  def inloadpath(dir : String) : value[Boolean]
  def mkcases(inductive : String) : value[List[List[String]]]
  def evars : value[Option[List[evar]]]
  def search(sf : search_flags) : value[List[coq_object[String]]]
  def get_options : value[List[(option_name, option_state)]]
  def set_options(
      options : List[(option_name, option_value)]) : value[Unit]
  def quit : value[Unit]
  /* ? */ def about : value[coq_info]

  def transaction[A](f : CoqTopIdeSlave_v20120710 => A) : value[A] = {
    val status = this.status match {
      case Good(s) => Some(s.status_statenum)
      case _ => None
    }
    try {
      CoqTypes.Good(f(transactionOverlay))
    } catch {
      case f : CoqFail =>
        interp(true, false, "BackTo " + status.getOrElse(1) + ".")
        CoqTypes.Fail[A](f.ep)
    }
  }

  def transactionOverlay() : CoqTopIdeSlave_v20120710 =
    new ExceptionalCoqTopIdeSlave_v20120710(this)
  def optionOverlay() : CoqTopIdeSlave_v20120710 =
    new OptionalCoqTopIdeSlave_v20120710(this)

  def getOption(name : option_name) : Option[option_state] =
      get_options match {
    case Good(options) => options.find(a => a._1 == name).map(_._2)
    case _ => None
  }
  def getOptionValue(name : option_name) : Option[option_value] =
    getOption(name).map(_.opt_value)
}
object CoqTopIdeSlave_v20120710 {
  def apply() : Option[CoqTopIdeSlave_v20120710] = apply(Seq.empty)

  def apply(args : Seq[String]) : Option[CoqTopIdeSlave_v20120710] = {
    val ct = new CoqTopIdeSlaveImpl(args)
    ct.about
    Some(ct.optionOverlay)
  }
}
