import cats.{Applicative, Monad}
import cats.syntax.all.*
import cats.effect.implicits.*

final case class Rendezvous[F[_], R <: SType](value: TChan[F, UChan[F]])
    extends AnyVal {

  def accept[A, Ctx <: CTypes, Y <: CTypes](
    f: [Tag] => Channel[F, Tag] => Session[F, Cap[Tag, EmptyTuple, R] *: Ctx, Y, A]
  )(using cg: ChannelGen[F], m: Monad[F]): Session[F, Ctx, Y, A] =
    Session[F, Ctx, Y, A](for {
      nc <- cg.makeUChan
      _ <- value.write(nc)
      res <- f(nc).run
    } yield res)

  def request[A, Ctx <: CTypes, Y <: CTypes](f: [Tag] => Channel[F, Tag] => Session[F, Cap[Tag, EmptyTuple, Dual[R]] *: Ctx, Y, A])(using
      m: Monad[F]
  ): Session[F, Ctx, Y, A] =
    Session[F, Ctx, Y, A](for {
      nc <- value.read
      res <- f(nc).run
    } yield res)
}

object Rendezvous {
  def apply[F[_]: Applicative, R <: SType](using
      cg: ChannelGen[F]
  ): F[Rendezvous[F, R]] =
    cg.makeTChan[UChan[F]].map(Rendezvous.apply)

}
