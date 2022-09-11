import cats.{Applicative, Monad}
import cats.syntax.all.*

sealed trait Nat

object Nat {
  sealed trait Z extends Nat
  sealed trait S[A <: Nat] extends Nat
}

// Session types
sealed trait SType
sealed trait Eps extends SType
sealed trait :!:[A, R <: SType] extends SType
sealed trait :?:[A, R <: SType] extends SType
sealed trait :+:[R <: SType, S <: SType] extends SType
sealed trait :&:[R <: SType, S <: SType] extends SType
sealed trait Rec[R <: SType] extends SType
sealed trait Var[V <: Nat] extends SType

type Dual[X <: SType] <: SType = X match {
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

// Phantom type for capabilities
sealed trait CType
sealed trait Cap[Tag, E <: Tuple, R <: SType] extends CType
sealed trait Closed extends CType

// Stack of capabilities
type CTypes = Tuple

final case class Session[F[_], S1 <: CTypes, S2 <: CTypes, A](run: F[A])
    extends AnyVal {

  def flatMapI[B, S3 <: CTypes](
      f: A => Session[F, S2, S3, B]
  )(using Monad[F]): Session[F, S1, S3, B] =
    Session[F, S1, S3, B](run.flatMap(x => f(x).run))

  def >>>[B, S3 <: CTypes](t: => Session[F, S2, S3, B])(using
      Monad[F]
  ): Session[F, S1, S3, B] =
    flatMapI(_ => t)
}

trait SessionDsl[F[_]: Monad] {

  // Capability stack
  type ECtx = EmptyTuple
  type SCtx[T] = T *: ECtx

  // Specialized session type to the effect type
  type Sess[S1 <: CTypes, S2 <: CTypes, A] = Session[F, S1, S2, A]

  private type C[Tag] = Channel[F, Tag]

  def pure[S <: CTypes, A](a: A): Sess[S, S, A] =
    Session[F, S, S, A](a.pure)

  def send[Tag, Env <: Tuple, Ctx <: CTypes, R <: SType, A](
      c: C[Tag],
      a: A
  ): Sess[Cap[Tag, Env, A :!: R] *: Ctx, Cap[Tag, Env, R] *: Ctx, Unit] =
    Session[F, Cap[Tag, Env, A :!: R] *: Ctx, Cap[Tag, Env, R] *: Ctx, Unit](
      c.write(a)
    )

  def recv[Tag, Env <: Tuple, Ctx <: CTypes, R <: SType, A](
      c: C[Tag]
  ): Session[F, Cap[Tag, Env, A :?: R] *: Ctx, Cap[Tag, Env, R] *: Ctx, A] =
    Session[F, Cap[Tag, Env, A :?: R] *: Ctx, Cap[Tag, Env, R] *: Ctx, A](
      c.read
    )

  def close[Tag, Env <: Tuple, Ctx <: CTypes](
      c: C[Tag]
  ): Session[F, Cap[Tag, Env, Eps] *: Ctx, Ctx, Unit] =
    Session[F, Cap[Tag, Env, Eps] *: Ctx, Ctx, Unit](().pure)

  def lift[S <: CTypes, A](fa: F[A]): Sess[S, S, A] =
    Session[F, S, S, A](fa)

  def sel1[Tag, Env <: Tuple, Ctx <: CTypes, R <: SType, S <: SType](
      c: C[Tag]
  ): Sess[Cap[Tag, Env, R :+: S] *: Ctx, Cap[Tag, Env, R] *: Ctx, Unit] =
    Session[F, Cap[Tag, Env, R :+: S] *: Ctx, Cap[Tag, Env, R] *: Ctx, Unit](
      c.write(true)
    )

  def sel2[Tag, Env <: Tuple, Ctx <: CTypes, R <: SType, S <: SType](
      c: C[Tag]
  ): Sess[Cap[Tag, Env, R :+: S] *: Ctx, Cap[Tag, Env, S] *: Ctx, Unit] =
    Session[F, Cap[Tag, Env, R :+: S] *: Ctx, Cap[Tag, Env, S] *: Ctx, Unit](
      c.write(false)
    )

  def offer[
      Tag,
      Env <: Tuple,
      Ctx <: CTypes,
      R <: SType,
      S <: SType,
      U <: CTypes,
      A
  ](
      c: C[Tag]
  )(
      s1: Sess[Cap[Tag, Env, R] *: Ctx, U, A],
      s2: Sess[Cap[Tag, Env, S] *: Ctx, U, A]
  ): Sess[Cap[Tag, Env, R :&: S] *: Ctx, U, A] =
    Session[F, Cap[Tag, Env, R :&: S] *: Ctx, U, A](
      for {
        b <- c.read[Boolean]
        res <- if b then s1.run else s2.run
      } yield res
    )

  def enter[Tag, Env <: Tuple, Ctx <: CTypes, R <: SType](
      c: C[Tag]
  ): Sess[Cap[Tag, Env, Rec[R]] *: Ctx, Cap[Tag, R *: Env, R] *: Ctx, Unit] =
    Session[
      F,
      Cap[Tag, Env, Rec[R]] *: Ctx,
      Cap[Tag, R *: Env, R] *: Ctx,
      Unit
    ](().pure)

  def zero[Tag, Env <: Tuple, Ctx <: CTypes, R <: SType](
      c: C[Tag]
  ): Sess[
    Cap[Tag, R *: Env, Var[Nat.Z]] *: Ctx,
    Cap[Tag, R *: Env, R] *: Ctx,
    Unit
  ] =
    Session[
      F,
      Cap[Tag, R *: Env, Var[Nat.Z]] *: Ctx,
      Cap[Tag, R *: Env, R] *: Ctx,
      Unit
    ](
      ().pure
    )

  def suc[
      Tag,
      Env1 <: Tuple,
      Env2 <: Tuple,
      Ctx <: CTypes,
      N <: Nat,
      R <: SType,
      S <: SType
  ](
      s: Sess[Cap[Tag, Env1, Var[N]] *: Ctx, Cap[Tag, Env2, S] *: Ctx, Unit]
  ): Sess[
    Cap[Tag, R *: Env1, Var[Nat.S[N]]] *: Ctx,
    Cap[Tag, Env2, S] *: Ctx,
    Unit
  ] =
    Session[
      F,
      Cap[Tag, R *: Env1, Var[Nat.S[N]]] *: Ctx,
      Cap[Tag, Env2, S] *: Ctx,
      Unit
    ](().pure)

  def dig[S1 <: CTypes, S2 <: CTypes, R <: CType, A](
      s: Sess[S1, S2, A]
  ): Sess[R *: S1, R *: S2, A] =
    Session[F, R *: S1, R *: S2, A](s.run)

  def swap[R <: CType, S <: CType, Ctx <: CTypes]
      : Sess[R *: S *: Ctx, S *: R *: Ctx, Unit] =
    Session[F, R *: S *: Ctx, S *: R *: Ctx, Unit](().pure)

}
