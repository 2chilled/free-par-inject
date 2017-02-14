package com.github.`2chilled`.todo

import java.time.ZonedDateTime
import scalaz.@@

object Domain {
  case class Todo(id: String @@ TodoId,
                  description: String,
                  createdDate: ZonedDateTime)

  sealed trait TodoId
}
