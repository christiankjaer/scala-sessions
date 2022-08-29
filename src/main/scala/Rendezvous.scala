import cats.{Applicative, Monad}
import cats.syntax.all.*
import cats.effect.implicits.*

final case class Rendezvous[F[_], R](value: TChan[F, UChan[F]]) extends AnyVal {

  def accept[A](
      f: Session[F, Cap[EmptyTuple, R], Unit, A]
  )(using cg: ChannelGen[F], m: Monad[F]) = for {
    nc <- cg.makeUChan
    _ <- value.write(nc)
    res <- f(nc)
  } yield res

  def request[A](f: Session[F, Cap[EmptyTuple, Dual[R]], Unit, A])(using
      m: Monad[F]
  ): F[A] =
    value.read.flatMap(f.apply)
}

object Rendezvous {
  def apply[F[_]: Applicative, R](using
      cg: ChannelGen[F]
  ): F[Rendezvous[F, R]] =
    cg.makeTChan[UChan[F]].map(Rendezvous.apply)

}
