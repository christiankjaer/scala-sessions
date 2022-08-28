import cats.effect.IO
import cats.syntax.all.*

final case class Rendezvous[R](value: TChan[UChan]) extends AnyVal

object Rendezvous {
  def apply[R](using cg: ChannelGen): IO[Rendezvous[R]] =
    cg.makeTChan[UChan].map(Rendezvous.apply)

  extension [R](r: Rendezvous[R])(using cg: ChannelGen) {
    def accept[A](f: Session[Cap[EmptyTuple, R], Unit, A]): IO[A] = for {
      nc <- cg.makeUChan
      _ <- r.value.write(nc)
      res <- f(nc)
    } yield res
  }

  extension [R](r: Rendezvous[R]) {
    def request[A](f: Session[Cap[EmptyTuple, Dual[R]], Unit, A]): IO[A] =
      r.value.read.flatMap(f.apply)
  }
}
