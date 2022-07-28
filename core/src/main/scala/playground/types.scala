package playground

import cats.data.Ior

object types {

  type IorThrow[+A] = Ior[Throwable, A]
}
