package com.github.`2chilled`

import com.github.`2chilled`.FreeUtils.liftFA
import com.github.`2chilled`.user.Domain.UserWithRole
import scala.language.higherKinds
import scalaz.concurrent.Task
import scalaz.{Inject, Free, FreeAp, @@, ~>}

object AuthZ {
  sealed trait AuthZAlgebra[A]

  case class User(name: String, password: String)

  sealed trait Resource

  case object Resource1 extends Resource

  object AuthZAlgebra {
    case class Authenticate(user: User) extends AuthZAlgebra[Option[UserWithRole]]

    case class Authorize(userWithRole: UserWithRole, resource: Resource) extends AuthZAlgebra[Boolean]
  }

  class Service[F[_]](implicit ev: Inject[AuthZAlgebra, F]) {
    def authenticate(name: String, password: String): FreeAp[F, Option[UserWithRole]] =
      liftFA(AuthZAlgebra.Authenticate(User(name, password)))
    def authorize(userWithRole: UserWithRole, resource: Resource) =
      liftFA(AuthZAlgebra.Authorize(userWithRole, resource))
  }

  object Service {
    implicit def authZInstance[F[_]](implicit ev: Inject[AuthZAlgebra, F]): Service[F] = new Service
  }

  val authZTaskInterpreter: AuthZAlgebra ~> Task = ???
}
