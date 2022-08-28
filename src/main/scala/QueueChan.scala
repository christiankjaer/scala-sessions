import cats.effect.std.Queue
import cats.effect.IO
import cats.effect.implicits.*
import cats.syntax.all.*

object QueueChan {
  def queueChanGen: ChannelGen = new ChannelGen {
    override def makeUChan: IO[UChan] =
      Queue
        .bounded[IO, Any](1)
        .map(q =>
          new UChan {
            override def read[T]: IO[T] = q.take.map(_.asInstanceOf[T])
            override def write[T](a: T): IO[Unit] = q.offer(a)
          }
        )
    override def makeTChan[T]: IO[TChan[T]] =
      Queue
        .bounded[IO, T](1)
        .map(q =>
          new TChan[T] {
            override def read: IO[T] = q.take
            override def write(a: T): IO[Unit] = q.offer(a)
          }
        )

  }
}
