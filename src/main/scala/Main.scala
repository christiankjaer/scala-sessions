import cats.effect.{ExitCode, IO, IOApp}
import scala.concurrent.duration.*

object Server {

  type PingPong = Int :!: Int :?: Eps

  val party1: Session[Cap[EmptyTuple, PingPong], Unit, Unit] =
    Session.send(42) >>> (Session.recv >>> Session.close)

  val party2: Session[Cap[EmptyTuple, Dual[PingPong]], Unit, Unit] =
    Session.recv.flatMapI(Session.send(_) >>> Session.close)

  type ServerProtocol = Eps :&: (String :?: Var[Nat.Z])

  val server: Session[Cap[
    EmptyTuple,
    Rec[ServerProtocol]
  ], Unit, Unit] = {

    def loop(): Session[
      Cap[ServerProtocol *: EmptyTuple, ServerProtocol],
      Unit,
      Unit
    ] = Session.offer(
      Session.close,
      Session.recv.flatMapI(s =>
        Session.lift(IO.println(s))
          >>> Session.zero >>> loop()
      )
    )
    Session.enter >>> loop()
  }

  val client
      : Session[Cap[EmptyTuple, Dual[Rec[ServerProtocol]]], Unit, Unit] = {

    def loop(i: Int): Session[
      Cap[Dual[ServerProtocol] *: EmptyTuple, Dual[ServerProtocol]],
      Unit,
      Unit
    ] =
      Session.lift(IO.readLine).flatMapI {
        case "q" =>
          Session.sel2 >>> Session.send(
            s"$i lines sent"
          ) >>> Session.zero >>> Session.sel1 >>> Session.close
        case s =>
          Session.sel2 >>> Session.send(s) >>> Session.zero >>> loop(i + 1)
      }

    Session.enter >>> loop(0)

  }

}

object Main extends IOApp {

  given ChannelGen = QueueChan.queueChanGen

  override def run(args: List[String]): IO[ExitCode] = for {
    rv <- Rendezvous.apply[Rec[Server.ServerProtocol]]
    f1 <- rv.accept(Server.server).start
    f2 <- rv.request(Server.client).start
    _ <- f1.join
    _ <- f2.join
  } yield ExitCode.Success

}
