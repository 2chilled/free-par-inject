package com.github.`2chilled`

import com.github.`2chilled`.FreeUtils.{liftFA}
import com.github.`2chilled`.user.Domain.UserWithRole
import org.log4s.Logger
import scala.language.higherKinds
import scalaz.concurrent.Task
import scalaz.{Inject, @@, ~>, FreeAp}

object Log {
  sealed trait LogAlgebra[A]

  object LogAlgebra {
    case class Debug(s: String, log: Logger) extends LogAlgebra[Unit]
    case class Info(s: String, log: Logger) extends LogAlgebra[Unit]
    case class Warn(s: String, t: Option[Throwable] = None, log: Logger) extends LogAlgebra[Unit]
    case class Error(s: String, t: Option[Throwable] = None, log: Logger) extends LogAlgebra[Unit]
  }

  class Service[F[_]](implicit ev: Inject[LogAlgebra, F]) {
    def debug(s: String)(implicit log: Logger): FreeAp[F, Unit] = liftFA(LogAlgebra.Debug(s, log))
    def info(s: String)(implicit log: Logger) = liftFA(LogAlgebra.Info(s, log))
    def warn(s: String, t: Option[Throwable] = None)(implicit log: Logger) = liftFA(LogAlgebra.Warn(s, t, log))
    def error(s: String, t: Option[Throwable] = None)(implicit log: Logger): FreeAp[F, Unit] = liftFA(LogAlgebra.Error(s, t, log))
  }

  object Service {
    implicit def logInstance[F[_]](implicit ev: Inject[LogAlgebra, F]): Service[F] = new Service
  }

  val logTaskInterpreter: LogAlgebra ~> Task = ???
}
