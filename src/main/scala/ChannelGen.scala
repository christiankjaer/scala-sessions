trait ChannelGen[F[_]] {
  def makeUChan: F[UChan[F]]
  def makeTChan[T]: F[TChan[F, T]]
}
