package scratchpad

import scratchpad.toplevel.Suite
import fs2._
import cats.effect._
import cats.implicits._
import cats.free._
import cats.free.FreeTMonad
import cats._
import weaver._

object HestTest extends SimpleIOSuite {
  pureTest("lopl") {
    expect(1 + 1 + 1 == 2)
  }
}

object MyTest extends FixtureSuite {
  override def tests: Suite[Test] =
    for {
      db <- fixture(
        Stream.resource(
          Resource
            .make(IO(println("acquiring db")))(_ => IO(println("releasing db")))
            .as(1)
        )
      )
      httpClient <- fixture(
        Stream.resource(
          Resource
            .make(IO(println("acquiring http client")))(_ =>
              IO(println("releasing http client"))
            )
            .as(2)
        )
      )
      t <- suite(
        test("test 1") { implicit ctx =>
          val c = httpClient.get
          ctx.lg.info("hey1") *> IO.pure(expect(true))
        },
        test("test 2") { ctx =>
          IO.pure(expect(1 + 1 + 1 == 2) and expect(false) and expect(5 > 5) and expect.same(1, 2) and expect.same(List(1,2,3), List(1,3)))
        }
      )
    } yield t
}