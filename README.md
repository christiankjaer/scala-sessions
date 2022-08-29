# scala-sessions

Implementation of session types with Scala 3 features.

## Example

A simple protocol for a ping pong protocol that will send a number, receive a number and close the channel.

```scala
type PingPong = Int :!: Int :?: Eps
```

One party can implement one side of the protocol
```scala
val party1: Sess[Cap[ECtx, PingPong], Unit, Unit] =
    send(42) >>> recv >>> close
```

And the other can implement the dual protocol that will echo back the received number.
```scala
val party2: Sess[Cap[ECtx, Dual[PingPong]], Unit, Unit] =
    recv.flatMapI(send(_) >>> close)
```

We can run the pair of processes with the `Rendezvous` service with an implicit generator
for new channels.
```scala
object Main extends IOApp {

  given ChannelGen[IO] = QueueChan.queueChanGen

  override def run(args: List[String]): IO[ExitCode] = for {
    rv <- Rendezvous.apply[IO, PingPont]
    f1 <- rv.accept(party1).start
    f2 <- rv.request(party2).start
    _ <- f1.join
    _ <- f2.join
  } yield ExitCode.Success
}
```
