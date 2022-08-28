import cats.effect.IO

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

trait TChan[A] {
  def write(a: A): IO[Unit]
  def read: IO[A]
}

trait UChan {
  def write[A](a: A): IO[Unit]
  def read[A]: IO[A]
}

sealed trait Cap[E, R]

final case class Session[S1, S2, A](s: UChan => IO[A]) extends AnyVal {
  def apply(c: UChan): IO[A] = s(c)
  def flatMapI[B, S3](
      f: A => Session[S2, S3, B]
  ): Session[S1, S3, B] =
    Session { c =>
      for {
        a <- s(c)
        res <- f(a)(c)
      } yield res
    }

  def >>>[B, S3](t: => Session[S2, S3, B]): Session[S1, S3, B] =
    flatMapI(_ => t)
}

object Session {
  def pure[S, A](a: A): Session[S, S, A] =
    Session(_ => IO.pure(a))

  def send[Ctx <: Tuple, R, A](
      a: A
  ): Session[Cap[Ctx, A :!: R], Cap[Ctx, R], Unit] =
    Session(_.write(a))

  def recv[Ctx <: Tuple, R, A]: Session[Cap[Ctx, A :?: R], Cap[Ctx, R], A] =
    Session(_.read)

  def close[Ctx <: Tuple]: Session[Cap[Ctx, Eps], Unit, Unit] =
    Session(_ => IO.unit)

  def lift[S, A](fa: IO[A]): Session[S, S, A] =
    Session(_ => fa)

  def sel1[Ctx <: Tuple, R, S]: Session[Cap[Ctx, R :+: S], Cap[Ctx, R], Unit] =
    Session(_.write(true))

  def sel2[Ctx <: Tuple, R, S]: Session[Cap[Ctx, R :+: S], Cap[Ctx, S], Unit] =
    Session(_.write(false))

  def offer[Ctx <: Tuple, R, S, U, A](
      s1: Session[Cap[Ctx, R], U, A],
      s2: Session[Cap[Ctx, S], U, A]
  ): Session[Cap[Ctx, R :&: S], U, A] =
    Session(c =>
      for {
        b <- c.read[Boolean]
        res <- if b then s1(c) else s2(c)
      } yield res
    )

  def enter[Ctx <: Tuple, R]
      : Session[Cap[Ctx, Rec[R]], Cap[R *: Ctx, R], Unit] =
    Session(_ => IO.unit)

  def zero[Ctx <: Tuple, R]
      : Session[Cap[R *: Ctx, Var[Nat.Z]], Cap[R *: Ctx, R], Unit] =
    Session(_ => IO.unit)

  def suc[Ctx <: Tuple, N <: Nat, R]
      : Session[Cap[R *: Ctx, Var[Nat.S[N]]], Cap[Ctx, Var[N]], Unit] =
    Session(_ => IO.unit)

}
