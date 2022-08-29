import cats.{Applicative, Monad}
import cats.syntax.all.*

sealed trait Nat

object Nat {
  sealed trait Z extends Nat
  sealed trait S[A <: Nat] extends Nat
}

sealed trait Eps
sealed trait :!:[A, R]
sealed trait :?:[A, R]
sealed trait :+:[R, S]
sealed trait :&:[R, S]
sealed trait Rec[R]
sealed trait Var[V]

type Dual[X] = X match {
  case (a :!: r) => a :?: Dual[r]
  case (a :?: r) => a :!: Dual[r]
  case (r :+: s) => Dual[r] :&: Dual[s]
  case (r :&: s) => Dual[r] :+: Dual[s]
  case Rec[r]    => Rec[Dual[r]]
  case Var[v]    => Var[v]
  case Eps       => Eps
}

trait TChan[F[_], A] {
  def write(a: A): F[Unit]
  def read: F[A]
}

trait UChan[F[_]] {
  def write[A](a: A): F[Unit]
  def read[A]: F[A]
}

sealed trait Cap[E, R]

final case class Session[F[_], S1, S2, A](s: UChan[F] => F[A]) extends AnyVal {
  def apply(c: UChan[F]): F[A] = s(c)
  def flatMapI[B, S3](
      f: A => Session[F, S2, S3, B]
  )(using Monad[F]): Session[F, S1, S3, B] =
    Session { c =>
      for {
        a <- s(c)
        res <- f(a)(c)
      } yield res
    }

  def >>>[B, S3](t: => Session[F, S2, S3, B])(using
      Monad[F]
  ): Session[F, S1, S3, B] =
    flatMapI(_ => t)
}

trait SessionDsl[F[_]: Monad] {

  type ECtx = EmptyTuple
  type SCtx[T] = T *: EmptyTuple

  type Sess[S1, S2, A] = Session[F, S1, S2, A]

  def pure[S, A](a: A): Sess[S, S, A] =
    Session(_ => a.pure)

  def send[Ctx <: Tuple, R, A](
      a: A
  ): Sess[Cap[Ctx, A :!: R], Cap[Ctx, R], Unit] =
    Session(_.write(a))

  def recv[Ctx <: Tuple, R, A]: Sess[Cap[Ctx, A :?: R], Cap[Ctx, R], A] =
    Session(_.read)

  def close[Ctx <: Tuple]: Sess[Cap[Ctx, Eps], Unit, Unit] =
    Session(_ => ().pure)

  def lift[S, A](fa: F[A]): Sess[S, S, A] =
    Session(_ => fa)

  def sel1[Ctx <: Tuple, R, S]
      : Sess[Cap[Ctx, R :+: S], Cap[Ctx, R], Unit] =
    Session(_.write(true))

  def sel2[Ctx <: Tuple, R, S]
      : Sess[Cap[Ctx, R :+: S], Cap[Ctx, S], Unit] =
    Session(_.write(false))

  def offer[Ctx <: Tuple, R, S, U, A](
      s1: Sess[Cap[Ctx, R], U, A],
      s2: Sess[Cap[Ctx, S], U, A]
  ): Sess[Cap[Ctx, R :&: S], U, A] =
    Session(c =>
      for {
        b <- c.read[Boolean]
        res <- if b then s1(c) else s2(c)
      } yield res
    )

  def enter[Ctx <: Tuple, R]
      : Sess[Cap[Ctx, Rec[R]], Cap[R *: Ctx, R], Unit] =
    Session(_ => ().pure)

  def zero[Ctx <: Tuple, R]
      : Sess[Cap[R *: Ctx, Var[Nat.Z]], Cap[R *: Ctx, R], Unit] =
    Session(_ => ().pure)

  def suc[Ctx <: Tuple, N <: Nat, R]
      : Sess[Cap[R *: Ctx, Var[Nat.S[N]]], Cap[Ctx, Var[N]], Unit] =
    Session(_ => ().pure)

}
