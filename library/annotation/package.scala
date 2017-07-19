/* Copyright 2015 EPFL, Lausanne */

package daisy

import scala.annotation.StaticAnnotation

package object annotation {
  
  @ignore
  class library    extends StaticAnnotation
  @ignore
  class verified   extends StaticAnnotation

  @ignore
  class main       extends StaticAnnotation
  @ignore
  class extern     extends StaticAnnotation
  @ignore
  class ignore     extends StaticAnnotation

}

