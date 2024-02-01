package scratchpad

import fs2._
import cats.effect._
import cats.implicits._
import cats.free._
import cats.free.FreeTMonad
import cats._

case object Db
case object Kafka
case object Http
case object BlobStore
case object Spice

object Attempt1 {
  case class TakeEvidence()

  trait Take[A] {
    def take(implicit ev: TakeEvidence): A
  }
  object Take {
    implicit lazy val monad: Monad[Take] =
      new Monad[Take] {
        override def pure[A](x: A): Take[A] =
          new Take[A] {
            def take(implicit ev: TakeEvidence): A = x
          }

        override def flatMap[A, B](fa: Take[A])(f: A => Take[B]): Take[B] =
          new Take[B] {
            def take(implicit ev: TakeEvidence): B = f(fa.take).take
          }

        override def tailRecM[A, B](a: A)(f: A => Take[Either[A, B]]): Take[B] =
          ???
      }
  }

  sealed trait Alg[A]
  final case class Allocate[A](
    s: Take[Stream[IO, A]]
  ) extends Alg[Take[A]] 
  type IOStream[A] = Stream[IO, A]
  type ResourceAlgebra[A] = FreeT[Alg, IOStream, A]
  object ResourceAlgebra {
    val runCompiler = {
        implicit val ev: TakeEvidence = TakeEvidence()
        new (Alg ~> IOStream) {
            def apply[A](fa: Alg[A]): IOStream[A] = fa match {
                case alg: Allocate[a] => 
                    alg.s.take.map{ a => 
                        new Take[a] {
                            def take(implicit ev: TakeEvidence): a = a
                        }
                    }
            }
        }
    }

    val analysisCompiler = new (Alg ~> IOStream) {
        def apply[A](fa: Alg[A]): IOStream[A] = fa match {
            case _: Allocate[a] => 
                Stream{
                    new Take[a] {
                        def take(implicit ev: TakeEvidence): a = throw new Exception("take called outside of test, how did this happen?")
                    }
                }
        }
    }
  }

  def fixture[A](stream: Take[Stream[IO, A]]): ResourceAlgebra[Take[A]] =
    FreeT.liftF(Allocate(stream))

  def fixture[A](stream: fs2.Stream[IO, A]): ResourceAlgebra[Take[A]] =
    fixture(stream.pure[Take])

  final case class TestResult()
  final case class Test(name: String, run: TakeEvidence => IO[TestResult])

  trait BaseSuite {
    def underlyingTests: ResourceAlgebra[Test]

    def test(name: String)(run: TakeEvidence => IO[TestResult]): Test =
      Test(name, run)

    def test(name: String)(run: IO[TestResult]): Test =
        test(name)(_ => run)

    def suite(tests: Test*): ResourceAlgebra[Test] =
        FreeT.liftT(Stream.emits(tests))

    def analyze: Stream[IO, Test] = underlyingTests.foldMap(ResourceAlgebra.analysisCompiler)

    def run: Stream[IO, TestResult] = {
      val ev: TakeEvidence = TakeEvidence()
      underlyingTests.foldMap(ResourceAlgebra.runCompiler).evalMap(_.run(ev))
    }
  }

  trait FixtureSuite extends BaseSuite {
    def tests: ResourceAlgebra[Test]

    def underlyingTests: ResourceAlgebra[Test] = tests
  }

  trait SimpleSuite extends BaseSuite {
    def tests: Stream[IO, Test]

    def underlyingTests: ResourceAlgebra[Test] = FreeT.liftT(tests)
  }

  object Fixtures {
    def db = fixture(Stream.emit(Db))
    def kafka = fixture(Stream.emit(Kafka))
    def http = fixture(Stream.emit(Http))
    def blobStore = fixture(Stream.emit(BlobStore))
    def spice(db: Take[Db.type]) = fixture(db.map(t => Stream.emit(Spice)))
  }

  class MyTest extends FixtureSuite {
    def tests = for {
      db <- Fixtures.db
      kafka <- Fixtures.kafka
      http <- Fixtures.http
      blobStore <- Fixtures.blobStore
      spice <- Fixtures.spice(db)
      t <- suite(
        test("test1") {
          IO.pure(TestResult())
        },
        test("test2") { implicit ev =>
          implicit val db0: Db.type = db.take
          implicit val kafka0: Kafka.type = kafka.take
          IO.pure(TestResult())
        }
      )
    } yield t
  }
}
