package com.github.`2chilled`.user

import com.github.`2chilled`.FreeUtils.liftFA
import com.github.`2chilled`.user.Domain._
import scala.language.higherKinds
import scalaz.concurrent.Task
import scalaz.{Inject, @@, ~>}

object Service {
  sealed trait UserAlgebra[A]

  object UserAlgebra {
    case class ReadUser(id: String @@ UserId) extends UserAlgebra[User]
    case class ReadUserWithRole(id: String @@ UserId) extends UserAlgebra[UserWithRole]
    case class WriteUser(user: User) extends UserAlgebra[User]
  }

  class Service[F[_]](implicit ev: Inject[UserAlgebra, F]) {
    def readUser(id: String @@ UserId) = liftFA(UserAlgebra.ReadUser(id))
    def readUserWithRole(id: String @@ UserId) = liftFA(UserAlgebra.ReadUserWithRole(id))
    def writeUser(user: User) = liftFA(UserAlgebra.WriteUser(user))
  }

  object Service {
    implicit def userInstance[F[_]](implicit ev: Inject[UserAlgebra, F]): Service[F] = new Service
  }

  val userTaskInterpreter: UserAlgebra ~> Task = ???
}
