import cats.effect.std.Queue
import cats.effect.IO
import cats.effect.Concurrent
import cats.effect.implicits.*
import cats.syntax.all.*

object QueueChan {
  def queueChanGen[F[_]: Concurrent]: ChannelGen[F] = new ChannelGen[F] {
    override def makeUChan: F[UChan[F]] =
      Queue
        .bounded[F, Any](1)
        .map(q =>
          new UChan[F] {
            override def read[T]: F[T] = q.take.map(_.asInstanceOf[T])
            override def write[T](a: T): F[Unit] = q.offer(a)
          }
        )
    override def makeTChan[T]: F[TChan[F, T]] =
      Queue
        .bounded[F, T](1)
        .map(q =>
          new TChan[F, T] {
            override def read: F[T] = q.take
            override def write(a: T): F[Unit] = q.offer(a)
          }
        )

  }
}
