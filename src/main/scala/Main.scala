import cats.effect.{ExitCode, IO, IOApp}
import cats.effect.implicits.*
import scala.concurrent.duration.*

object Server extends SessionDsl[IO] {

  type PingPong = Int :!: Int :?: Eps

  val party1: Sess[Cap[ECtx, PingPong], Unit, Unit] =
    send(42) >>> recv >>> close

  val party2: Sess[Cap[ECtx, Dual[PingPong]], Unit, Unit] =
    recv.flatMapI(send(_) >>> close)

  type ServerProtocol = Eps :&: (String :?: Var[Nat.Z])

  val server: Sess[Cap[ECtx, Rec[ServerProtocol]], Unit, Unit] = {

    def loop(): Sess[Cap[SCtx[ServerProtocol], ServerProtocol], Unit, Unit] =
      offer(
        close,
        recv.flatMapI(s => lift(IO.println(s)) >>> zero >>> loop())
      )
    enter >>> loop()
  }

  val client: Sess[Cap[ECtx, Dual[Rec[ServerProtocol]]], Unit, Unit] = {

    def loop(
        i: Int
    ): Sess[Cap[SCtx[Dual[ServerProtocol]], Dual[ServerProtocol]], Unit, Unit] =
      lift(IO.readLine).flatMapI {
        case "q" =>
          sel2 >>> send(s"$i lines sent") >>> zero >>> sel1 >>> close
        case s =>
          sel2 >>> send(s) >>> zero >>> loop(i + 1)
      }

    enter >>> loop(0)
  }
}

object Main extends IOApp {

  given ChannelGen[IO] = QueueChan.queueChanGen

  override def run(args: List[String]): IO[ExitCode] = for {
    rv <- Rendezvous.apply[IO, Rec[Server.ServerProtocol]]
    f1 <- rv.accept(Server.server).start
    f2 <- rv.request(Server.client).start
    _ <- f1.join
    _ <- f2.join
  } yield ExitCode.Success

}
