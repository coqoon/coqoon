/* CastUtilities.scala
 * Utility methods for (possibly unsuccessfully) casting objects
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

package dk.itu.coqoon.core.utilities

object TryCast {
  def apply[A](a : Any)(implicit a0 : Manifest[A]) = a0.unapply(a)
}

object TryAdapt {
  import org.eclipse.core.runtime.IAdaptable
  def apply[A](ad : IAdaptable)(implicit a0 : Manifest[A]) : Option[A] =
    Option(ad).map(_.getAdapter(a0.runtimeClass)).flatMap(TryCast[A])
}

object TryService {
  import org.eclipse.ui.services.IServiceLocator
  def apply[A](sl : IServiceLocator)(implicit a0 : Manifest[A]) : Option[A] =
    Option(sl).map(_.getService(a0.runtimeClass)).flatMap(TryCast[A])
}