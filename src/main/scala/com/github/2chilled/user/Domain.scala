package com.github.`2chilled`.user

import scalaz.@@

object Domain {
  case class User(id: String @@ UserId,
                  name: String,
                  password: String)

  case class Role(name: String)

  case class UserWithRole(user: User, role: Role)

  sealed trait UserId
}
