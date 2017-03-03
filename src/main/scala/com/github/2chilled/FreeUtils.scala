package com.github.`2chilled`

import scala.language.higherKinds
import scalaz.syntax.tag._
import scalaz.{Free, FreeAp, Inject, Coproduct, ~>, Tag, Tags, BindRec, Monad, \/, Nondeterminism, @@}

//TODO use kind projector plugin to avoid type lambdas
object FreeUtils {
  outer =>

  implicit def freeParMonad[S[_]]: Monad[({type fp[X] = FreePar[S, X]})#fp] with BindRec[({type fp[X] = FreePar[S, X]})#fp] = {
    type fp[X] = FreePar[S, X]

    new Monad[fp] with BindRec[fp] {
      override def map[A, B](fa: fp[A])(f: A => B) = fa map f
      def bind[A, B](a: fp[A])(f: A => fp[B]) = a flatMap f
      def point[A](a: => A) = FreeAp.point(a).liftPar
      def tailrecM[A, B](f: A => fp[A \/ B])(a: A) =
        f(a).flatMap(_.fold(tailrecM(f), point(_)))
    }
  }

  /**
   * Sequence of F commands embedded in FreeAp. This way the interpreter is free to interpret each suspension
   * in parallel.
   *
   * Note that the interpreter has no chance to do so for a plain Free[F, A]
   */
  type FreePar[F[_], A] = Free[({type X[T] = FreeAp[F, T]})#X, A]

  /**
   * Lift a command in the algebra F into the Free Applicative FreeAp[G, A], where G is a superset of F
   * witnessed by Inject[F, G].
   *
   * Note: Always use this lifting for your smart constructor for F.
   */
  def liftFA[F[_], G[_], A](fa: F[A])(implicit inj: Inject[F, G]): FreeAp[G, A] =
    FreeAp.lift(inj inj fa)

  /**
   * Lift an applicative program into a sequential one consisting of parallel suspensions.
   */
  def liftPar[F[_], A](fa: FreeAp[F, A]): FreePar[F, A] =
    Free.liftF[({type fa[T] = FreeAp[F, T]})#fa, A](fa)

  /**
   * Lift an sequential program into FreePar where each FreeAp consists of a single suspension.
   */
  def liftSeq[F[_], A](fa: Free[F, A]): FreePar[F, A] =
    fa.mapSuspension[({type fa[T] = FreeAp[F, T]})#fa](new (F ~> ({type fa[T] = FreeAp[F, T]})#fa) {
      def apply[T](fa: F[T]): FreeAp[F, T] = liftFA(fa)
    })

  //TODO why isn't that part of scalaz?
  implicit class NTOps[F[_], G[_]](nt: F ~> G) {
    /**
     * Combine this NT with another one having the same codomain, in a way that the result NT's domain is the coproduct
     * of the two original NT's domains. Useful for combining interpreters for different algebra's to be able to
     * interpret free programs making use of different algebras.
     */
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

  /**
   * Given a NAT, modifies it's domain to accept the same one lifted into FreeAp. The resulting interpreter
   * evaluates each FreeAp step in parallel, utilizing the Nondeterminism witness for G.
   *
   * Useful for turning an interpreter for an algebra F into one that is able to interpret FreeAp[F, ?] to G
   * in parallel.
   */
  def parInterpret[F[_], G[_]](nat: F ~> G)
                              (implicit nonD: Nondeterminism[G]): ({type fa[T] = FreeAp[F, T]})#fa ~> G =
    new (({type fa[T] = FreeAp[F, T]})#fa ~> G) {
      def apply[A](f: FreeAp[F, A]): G[A] = {

        type GPar[X] = G[X] @@ Tags.Parallel

        val natPar = nat andThen (new (G ~> GPar) {
          def apply[B](gb: G[B]) = Tag.of[Tags.Parallel](gb)
        })

        implicit val gParInstance = nonD.parallel

        val parG = f foldMap natPar

        parG.unwrap
      }
    }
}

