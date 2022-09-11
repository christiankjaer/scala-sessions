import cats.effect.{ExitCode, IO, IOApp}
import cats.effect.implicits.*
import scala.concurrent.duration.*

object Server extends SessionDsl[IO] {

  type PingPong = Int :!: Int :?: Eps

  def party1[Tag, Ctx <: CTypes](
      c: Channel[IO, Tag]
  ): Sess[Cap[Tag, ECtx, PingPong] *: Ctx, Ctx, Int] =
    send(c, 42) >>> recv(c).flatMapI(i => close(c) >>> lift(IO.pure(i)))

  def party2[Tag, Ctx <: CTypes](
      c: Channel[IO, Tag]
  ): Sess[Cap[Tag, ECtx, Dual[PingPong]] *: Ctx, Ctx, Unit] =
    recv(c).flatMapI(send(c, _) >>> close(c))

  type ServerProtocol = Eps :&: (String :?: Var[Nat.Z])

  def server[Tag, Ctx <: CTypes](
      c: Channel[IO, Tag]
  ): Sess[Cap[Tag, ECtx, Rec[ServerProtocol]] *: Ctx, Ctx, Unit] = {

    def loop(): Sess[
      Cap[Tag, SCtx[ServerProtocol], ServerProtocol] *: Ctx,
      Ctx,
      Unit
    ] =
      offer(c)(
        close(c),
        recv(c).flatMapI(s => lift(IO.println(s)) >>> zero(c) >>> loop())
      )

    enter(c) >>> loop()
  }

  def client[Tag, Ctx <: CTypes](
      c: Channel[IO, Tag]
  ): Sess[Cap[Tag, ECtx, Rec[Dual[ServerProtocol]]] *: Ctx, Ctx, Unit] = {

    def loop(
        i: Int
    ): Sess[
      Cap[Tag, SCtx[Dual[ServerProtocol]], Dual[ServerProtocol]] *: Ctx,
      Ctx,
      Unit
    ] =
      lift(IO.readLine).flatMapI {
        case "q" =>
          sel2(c) >>> send(c, s"$i lines sent") >>> zero(c) >>> sel1(
            c
          ) >>> close(c)
        case s =>
          sel2(c) >>> send(c, s) >>> zero(c) >>> loop(i + 1)
      }

    enter(c) >>> loop(0)
  }
}

object Main extends IOApp {

  given ChannelGen[IO] = QueueChan.queueChanGen

  override def run(args: List[String]): IO[ExitCode] = for {
    rv <- Rendezvous.apply[IO, Rec[Server.ServerProtocol]]
    f1 <- rv
      .accept(
        [Tag] => (c: Channel[IO, Tag]) => Server.server[Tag, EmptyTuple](c)
      )
      .run
      .start
    f2 <- rv
      .request(
        [Tag] => (c: Channel[IO, Tag]) => Server.client[Tag, EmptyTuple](c)
      )
      .run
      .start
    _ <- f1.join.flatMap(IO.println)
    _ <- f2.join.flatMap(IO.println)
  } yield ExitCode.Success

}
