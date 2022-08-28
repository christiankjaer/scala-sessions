import cats.effect.IO

trait ChannelGen {
  def makeUChan: IO[UChan]
  def makeTChan[T]: IO[TChan[T]]
}
