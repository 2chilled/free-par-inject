package com.github.`2chilled`.todo

import com.github.`2chilled`.FreeUtils.{liftFA}
import com.github.`2chilled`.todo.Domain.Todo
import com.github.`2chilled`.user.Domain.UserId
import scala.language.higherKinds
import scalaz.concurrent.Task
import scalaz.{Inject, @@, ~>}

object Service {
  sealed trait TodoAlgebra[A]

  object TodoAlgebra {
    case class ReadTodos(userId: String @@ UserId) extends TodoAlgebra[List[Todo]]
    case class AddTodo(userId: String @@ UserId, todo: Todo) extends TodoAlgebra[Todo]
  }

  class Service[F[_]](implicit ev: Inject[TodoAlgebra, F]) {
    def readTodos(userId: String @@ UserId) = liftFA(TodoAlgebra.ReadTodos(userId))
    def addTodo(userId: String @@ UserId, todo: Todo) = liftFA(TodoAlgebra.AddTodo(userId, todo))
  }

  object Service {
    implicit def todoInstance[F[_]](implicit ev: Inject[TodoAlgebra, F]): Service[F] = new Service
  }

  val todoTaskInterpreter: TodoAlgebra ~> Task = ???
}
