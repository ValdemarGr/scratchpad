package scratchpad

import cats.effect._
import fs2.Stream
import org.testcontainers.utility.DockerImageName
import toplevel._
import cats.implicits._
import org.testcontainers.containers.BindMode
import org.testcontainers.containers.{GenericContainer, Container}
import org.testcontainers.containers.startupcheck.OneShotStartupCheckStrategy
import scala.concurrent.duration._
import scala.jdk.DurationConverters._

object Fixtures {
  import SuiteOps._

  final case class Postgres(psql: GenericContainer[?]) {
    def host = psql.getHost()
    def port = psql.getMappedPort(5432).toInt
  }
  def postgres: Suite[Fixture[Postgres]] = {
    import org.testcontainers.containers.wait.strategy.Wait
    fixture {
      Stream.resource {
        ContainerBuilder("postgres:14")
          .modify(_.addEnv("POSTGRES_USER", "postgres"))
          .modify(_.addEnv("POSTGRES_PASSWORD", "1234"))
          .modify(_.addEnv("POSTGRES_DB", "postgres"))
          .modify(_.addEnv("POSTGRES_HOST_AUTH_METHOD", "trust"))
          .modify(_.addExposedPort(5432))
          .modify(
            _.setWaitStrategy(
              Wait.forSuccessfulCommand("pg_isready -U postgres")
            )
          )
          .start[IO]
          .map(Postgres(_))
      }
    }
  }

  def migrationContainer(migrationsPath: fs2.io.file.Path,db: Postgres): ContainerBuilder = 
    ContainerBuilder("migrate/migrate")
      .modify(
        _.addFileSystemBind(
          migrationsPath.absolute.toString,
          "/migrations",
          BindMode.READ_ONLY
        )
      )
      .modify(
        _.withCommand(
          "-path=/migrations/",
          "-database",
          s"postgres://postgres:1234@${db.host}:${db.port}/postgres?sslmode=disable",
          "up"
        )
      )
      .modify(_.setStartupCheckStrategy(new OneShotStartupCheckStrategy().withTimeout(10.seconds.toJava)))

  def migrate(migrationsPath: fs2.io.file.Path,db: Fixture[Postgres]): Suite[Unit] =
    fixture(db.map(db => Stream.resource(migrationContainer(migrationsPath, db).start[IO]))).void

  def migratedPostgres(migrationsPath: fs2.io.file.Path): Suite[Fixture[Postgres]] =
    postgres.flatTap(migrate(migrationsPath, _))

  final case class Redpanda(rpk: GenericContainer[?]) {
    def host = rpk.getHost()
    def port = rpk.getMappedPort(9092).toInt
  }
  def redpanda: Suite[Fixture[Redpanda]] = {
    import GenericContainer._
    val cmd =
      s"""|rpk config bootstrap --id 0 && \\
          |rpk config set redpanda.seed_servers "[]" --format json && \\
          |rpk config set rpk.additional_start_flags "[\\"--smp=1\\",\\"--memory=500M\\",\\"--reserve-memory=0M\\",\\"--default-log-level=debug\\"]" --format json && \\
          |rpk config set pandaproxy.pandaproxy_api "[{\\"address\\":\\"0.0.0.0\\",\\"port\\":8082}]" --format json && \\
          |rpk config set pandaproxy.advertised_pandaproxy_api "[{\\"address\\":\\"redpanda\\",\\"port\\":8082}]" --format json && \\
          |rpk config set redpanda.kafka_api "[{\\"address\\":\\"0.0.0.0\\",\\"port\\":9092}]" --format json && \\
          |rpk config set redpanda.advertised_kafka_api "[{\\"address\\":\\"127.0.0.1\\",\\"port\\":9092}]" --format json && \\
          |rpk config set redpanda.rpc_server "{\\"address\\":\\"0.0.0.0\\",\\"port\\":33145}" --format yaml && \\
          |rpk config set redpanda.advertised_rpc_api "{\\"address\\":\\"redpanda\\",\\"port\\":33145}" --format json && \\
          |rpk redpanda start --overprovisioned --check=false""".stripMargin
    fixture {
      Stream.resource {
        ContainerBuilder("docker.vectorized.io/vectorized/redpanda:v21.11.12")
          .modify(_.withCreateContainerCmdModifier(_.withEntrypoint("bash", "-c", cmd)))
          .modify(_.addExposedPort(9092))
          .start[IO]
      }
    }.map(_.map(Redpanda(_)))
  }

  def sftp(key: fs2.io.file.Path) = {

  }
}
