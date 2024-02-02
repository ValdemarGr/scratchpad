package scratchpad

import fs2._
import cats.effect._
import cats.implicits._
import cats.free._
import cats.free.FreeTMonad
import cats._
import org.typelevel.vault._
import weaver.{TestOutcome, TestName, RunnableSuite, UnsafeRun,Expectations, Test => WTest, Log}
import cats.effect.unsafe.IORuntime
import weaver.CatsUnsafeRun
import weaver.BaseCatsSuite
import weaver.EffectSuite

sealed trait TestContext
object TestContext {
  final case class FixtureCtx() extends TestContext
  final case class TestCtx(lg: Log[IO]) extends TestContext
}

trait Fixture[A] {
  type A0 = A
  def get(implicit context: TestContext): A
}
object Fixture {
  def apply[A](f: TestContext => A) = new Fixture[A] {
    def get(implicit context: TestContext): A = f(context)
  }
  implicit lazy val monad: Monad[Fixture] = new Monad[Fixture] {
    override def pure[A](x: A): Fixture[A] = Fixture(_ => x)
    override def flatMap[A, B](fa: Fixture[A])(f: A => Fixture[B]): Fixture[B] =
      Fixture(implicit ctx => f(fa.get).get)

    override def tailRecM[A, B](a: A)(
        f: A => Fixture[Either[A, B]]
    ): Fixture[B] = Fixture { implicit ctx =>
      def go(x: Fixture[Either[A, B]]): Eval[B] = Eval.defer {
        x.get match {
          case Left(a)  => go(f(a))
          case Right(b) => Eval.now(b)
        }
      }
      go(f(a)).value
    }
  }
}

sealed trait SuiteAlgebra[A]
final case class Allocate[A](s: Fixture[Stream[IO, A]])
    extends SuiteAlgebra[Fixture[A]]

object toplevel {
  type IOStream[A] = Stream[IO, A]
  type Suite[A] = FreeT[SuiteAlgebra, IOStream, A]
  object Suite {
    def runCompiler(implicit context: TestContext) =
      new (SuiteAlgebra ~> IOStream) {
        def apply[A](fa: SuiteAlgebra[A]): IOStream[A] = fa match {
          case alg: Allocate[a] => alg.s.get.map(a => Fixture[a](_ => a))
        }
      }

    val analysisCompiler = new (SuiteAlgebra ~> IOStream) {
      def apply[A](fa: SuiteAlgebra[A]): IOStream[A] = fa match {
        case _: Allocate[a] =>
          val msg = "take called outside of test, how did this happen?"
          Stream(Fixture[a](_ => throw new Exception(msg)))
      }
    }
  }
}
import toplevel._

final case class Test(name: TestName, run: TestContext.FixtureCtx => IO[TestOutcome])

trait SuiteOps {
  class PartiallyAppliedTest(name: TestName) {
    def apply(run: TestContext.TestCtx => IO[Expectations]): Test =
      Test(name, x => WTest[IO](name.name, (lg: Log[IO]) => run(TestContext.TestCtx(lg)))(CatsUnsafeRun))

    def apply(run: IO[Expectations]): Test =
      apply(_ => run)
  }

  def test(name: TestName): PartiallyAppliedTest = new PartiallyAppliedTest(name)
  
  def suite(tests: Test*): Suite[Test] = FreeT.liftT(Stream.emits(tests))
  
  def fixture[A](stream: Fixture[Stream[IO, A]]): Suite[Fixture[A]] =
    FreeT.liftF[SuiteAlgebra, IOStream, Fixture[A]](Allocate(stream))

  def fixture[A](stream: Stream[IO, A]): Suite[Fixture[A]] =
    fixture(stream.pure[Fixture])
}
object SuiteOps extends SuiteOps

trait BaseSuite extends SuiteOps {
  def underlyingSuite: Suite[Test]

  def analyze: Stream[IO, Test] =
    underlyingSuite.foldMap(Suite.analysisCompiler)

  def outcomes: Stream[IO, TestOutcome] = {
    implicit val ev: TestContext.FixtureCtx = TestContext.FixtureCtx()
    underlyingSuite.foldMap(Suite.runCompiler).evalMap(_.run(ev))
  }
}

trait WeaverRTSuite extends RunnableSuite[IO] with BaseSuite with Expectations.Helpers with BaseCatsSuite {
  override def getSuite: EffectSuite[IO] = this
  override def spec(args: List[String]): Stream[IO, TestOutcome] = outcomes
  override protected implicit def effectCompat: UnsafeRun[IO] = CatsUnsafeRun
  override def plan: List[TestName] =
    analyze.map(_.name).compile.toList.unsafeRunSync()(IORuntime.global)
}

trait FixtureSuite extends WeaverRTSuite {
  def tests: Suite[Test]

  def underlyingSuite: Suite[Test] = tests
}

trait SimpleSuite extends WeaverRTSuite {
  def tests: Stream[IO, Test]

  def underlyingSuite: Suite[Test] = FreeT.liftT(tests)
}