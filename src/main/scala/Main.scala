import cats.effect.{ExitCode, IO, IOApp}
import cats.effect.implicits.*

object Server extends SessionDsl[IO] {

  type PingPong = Int :!: Int :?: Eps

  def party1[Tag, Ctx <: CTypes](
      c: Channel[IO, Tag]
  ): Sess[Cap[Tag, EmptyTuple, PingPong] *: Ctx, Ctx, Int] =
    send(c)(42) >>> recv(c).flatMapI(i => close(c) >>> lift(IO.pure(i)))

  def party2[Tag, Ctx <: CTypes](
      c: Channel[IO, Tag]
  ): Sess[Cap[Tag, EmptyTuple, Dual[PingPong]] *: Ctx, Ctx, Unit] =
    recv(c).flatMapI(send(c)(_) >>> close(c))

  type ServerProtocol = Eps :&: (String :?: Var["R"])

  def server[Tag, Ctx <: CTypes](
      c: Channel[IO, Tag]
  ): Sess[Cap[Tag, EmptyTuple, Rec["R", ServerProtocol]] *: Ctx, Ctx, Unit] = {

    type Env = ("R", ServerProtocol) *: EmptyTuple

    def loop(): Sess[Cap[Tag, Env, ServerProtocol] *: Ctx, Ctx, Unit] =
      offer(c)(
        close(c),
        recv(c).flatMapI(s =>
          lift(IO.println(s)) >>> call(c)["R", Env] >>> loop()
        )
      )

    enter(c) >>> loop()
  }

  type ClientProtocol = Dual[ServerProtocol]

  def client[Tag, Ctx <: CTypes](
      c: Channel[IO, Tag]
  ): Sess[Cap[Tag, EmptyTuple, Rec["R", ClientProtocol]] *: Ctx, Ctx, Unit] = {

    type Env = ("R", ClientProtocol) *: EmptyTuple

    def loop(i: Int): Sess[Cap[Tag, Env, ClientProtocol] *: Ctx, Ctx, Unit] =
      lift(IO.readLine).flatMapI {
        case "q" =>
          sel2(c) >>> send(c)(s"$i lines sent") >>> call(c)["R", Env] >>>
            sel1(c) >>> close(c)

        case s => sel2(c) >>> send(c)(s) >>> call(c)["R", Env] >>> loop(i + 1)
      }

    enter(c) >>> loop(0)
  }
}

object Main extends IOApp {

  given ChannelGen[IO] = QueueChan.queueChanGen

  override def run(args: List[String]): IO[ExitCode] = for {
    rv <- Rendezvous.apply[IO, Rec["R", Server.ServerProtocol]]
    f1 <- rv
      .accept([T] => (c: Channel[IO, T]) => Server.server[T, EmptyTuple](c))
      .run
      .start
    f2 <- rv
      .request([T] => (c: Channel[IO, T]) => Server.client[T, EmptyTuple](c))
      .run
      .start
    _ <- f1.join.flatMap(IO.println)
    _ <- f2.join.flatMap(IO.println)
  } yield ExitCode.Success

}
