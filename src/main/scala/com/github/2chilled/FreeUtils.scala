package com.github.`2chilled`

import scala.language.higherKinds
import scalaz.{Free, FreeAp, Unapply, Inject, Coproduct, ~>}

//TODO use kind projector plugin
object FreeUtils {
  outer =>

  type FreePar[F[_], A] = Free[({type X[T] = FreeAp[F, T]})#X, A]

  def liftFA[F[_], G[_], A](fa: F[A])(implicit inj: Inject[F, G]): FreeAp[G, A] =
    FreeAp.lift(inj inj fa)

  def liftPar[F[_], A](fa: FreeAp[F, A]): FreePar[F, A] =
    Free.liftF[({type X[T] = FreeAp[F, T]})#X, A](fa)

  def liftSeq[F[_], A](fa: Free[F, A]): FreePar[F, A] =
    fa.mapSuspension[({type X[T] = FreeAp[F, T]})#X](new (F ~> ({type X[T] = FreeAp[F, T]})#X) {
      def apply[A](fa: F[A]): FreeAp[F, A] = liftFA(fa)
    })

  //def liftU[FA](fa: FA)(implicit inj: Inject[F, G]): FreeAp[G, A] =
    //FreeAp.lift(inj inj fa)

  //TODO why isn't that part of scalaz?
  implicit class NTOps[F[_], G[_]](nt: F ~> G) {
    def or[H[_]](ont: H ~> G):(({type cp[P]=Coproduct[F,H,P]})#cp ~> G) = new (({type cp[P]=Coproduct[F,H,P]})#cp ~> G) {
      def apply[A](fa: Coproduct[F, H, A]): G[A] = fa.run.fold(nt(_), ont(_))
    }
  }

  implicit class FreeApSyntax[F[_], A](fa: FreeAp[F, A]) {
    def liftPar: FreePar[F, A] = outer.liftPar(fa)
  }

  implicit class FreeSyntax[F[_], A](fa: Free[F, A]) {
    def liftSeq: FreePar[F, A] = outer.liftSeq(fa)
  }
}
