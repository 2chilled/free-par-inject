package com.github.`2chilled`

import com.github.`2chilled`.AuthZ.{Service => AuthZService, AuthZAlgebra, authZTaskInterpreter}
import com.github.`2chilled`.Log.{Service => LogService, LogAlgebra, logTaskInterpreter}
//import com.github.`2chilled`.Par.{Service => ParService}
import com.github.`2chilled`.FreeUtils.{FreePar, NTOps, FreeApSyntax, FreeSyntax}
import com.github.`2chilled`.todo.Domain.{Todo, TodoId}
import com.github.`2chilled`.todo.Service.{Service => TodoService, TodoAlgebra, todoTaskInterpreter}
import com.github.`2chilled`.user.Domain.{UserWithRole, Role}
import com.github.`2chilled`.user.Service.{Service => UserService, UserAlgebra, userTaskInterpreter}
import java.time.ZonedDateTime
import org.log4s._
import scala.language.higherKinds
import scalaz.concurrent.Task
import scalaz.syntax.tag._
import scalaz.{Free, FreeAp, Coproduct, Tag, ~>, Monad, Apply, Tags, @@}

object Main extends App {
  private implicit lazy val log: org.log4s.Logger = getLogger

  def addTodo[F[_]](userName: String,
                    password: String,
                    todo: Todo)
                   (implicit userS: UserService[F],
                    todoS: TodoService[F],
                    authZ: AuthZService[F],
                    logS: LogService[F]): FreePar[F, Unit] = for {
    userWithRoleOption <- authZ.authenticate(userName, password).liftPar
    _ <- userWithRoleOption.map(u => addTodoHelper(todo, u)).getOrElse(logS.error(s"user $userName is not a known user").liftPar)
    _ <- logSeq.liftSeq
    _ <- logPar.liftPar
  } yield ()

  private def logSeq[F[_]](implicit logS: LogService[F]): Free[F, Unit] =
    logS.debug("Logged in Free").monadic

  private def logPar[F[_]](implicit logS: LogService[F]): FreeAp[F, Unit] =
    Apply[({type fa[T] = FreeAp[F, T]})#fa]
      .tuple2(logS.debug("Logged in FreeAp 1"), logS.debug("Logged in FreeAp 2"))
      .map(_ => ())

  private def addTodoHelper[F[_]](todo: Todo, u: UserWithRole)
                                 (implicit todoS: TodoService[F],
                                  logS: LogService[F]): FreePar[F, Unit] = {
    type X[T] = FreeAp[F, T]

    for {
      _ <- logS.debug(s"program2, user = $u").liftPar
      todo0 <- todoS.addTodo(u.user.id, todo).liftPar
      _ <- logS.debug(s"added $todo0 for user $u").liftPar
      allTodos <- todoS.readTodos(u.user.id).liftPar
      _ <- Apply[X].tuple2(todoS.readTodos(u.user.id), todoS.readTodos(u.user.id)).liftPar
      _ <- logS.debug(s"all todos of $u: $allTodos").liftPar
    } yield ()
  }

  type T1[A] = Coproduct[UserAlgebra, TodoAlgebra, A]

  type T2[A] = Coproduct[AuthZAlgebra, T1, A]

  type T3[A] = Coproduct[LogAlgebra, T2, A]

  type App[A] = T3[A]

  def parInterpret[F[_]](nat: F ~> Task): ({type X[T] = FreeAp[F, T]})#X ~> Task = {
    val parTaskNat = new (({type X[T] = FreeAp[F, T]})#X ~> Task.ParallelTask) {
      val taskParTaskNat = new (Task ~> Task.ParallelTask) {
        def apply[A](f: Task[A]): Task.ParallelTask[A] = Tag.of[Tags.Parallel](f)
      }

      import Task.taskParallelApplicativeInstance

      def apply[A](f: FreeAp[F, A]): Task.ParallelTask[A] = f foldMap (nat andThen taskParTaskNat)
    }

    val parTaskTaskNat = new (Task.ParallelTask ~> Task) {
      def apply[A](t: Task.ParallelTask[A]): Task[A] = t.unwrap
    }

    parTaskNat andThen parTaskTaskNat
  }

  val appTaskInterpreter: App ~> Task = {
    val t1: T1 ~> Task = userTaskInterpreter or todoTaskInterpreter
    val t2: T2 ~> Task = authZTaskInterpreter or t1
    val t3: T3 ~> Task = logTaskInterpreter or t2
    t3
  }

  //val parAppTaskInterpreter: ({type X[T] = FreeAp[F, T]})#X ~> Task = {
    //val t1: T1 ~> Task = userTaskInterpreter or todoTaskInterpreter
    //val t2: T2 ~> Task = authZTaskInterpreter or t1
    //val t3: T3 ~> Task = logTaskInterpreter or t2
    //t3
  //}

  val program = addTodo[App](
    userName = "Bernd",
    password = "secret",
    todo = Todo(
      id = Tag.of[TodoId]("testId"),
      description = "",
      createdDate = ZonedDateTime.now
    )
  )

  program.foldMap(parInterpret(appTaskInterpreter)).unsafePerformSync
}
