package scratchpad

import cats.effect._
import fs2.Stream
import org.testcontainers.utility.DockerImageName
import toplevel._
import cats.implicits._
import org.testcontainers.containers.BindMode
import org.testcontainers.containers.{Container, GenericContainer}
import org.testcontainers.containers.startupcheck.OneShotStartupCheckStrategy
import scala.concurrent.duration._
import scala.jdk.DurationConverters._
import org.testcontainers.containers.Network

object Fixtures {
  import SuiteOps._

  def network: Suite[Fixture[Network]] = fixture {
    Stream.bracket(IO.blocking(Network.newNetwork()))(n => IO.blocking(n.close()))
  }

  final case class Postgres(psql: GenericContainer[?]) {
    def host = psql.getHost()
    def port = psql.getMappedPort(5432).toInt
  }
  def postgres(network: Fixture[Network]): Suite[Fixture[Postgres]] = {
    import org.testcontainers.containers.wait.strategy.Wait
    fixture {
      network.map { n =>
        Stream.resource {
          ContainerBuilder("postgres:14")
            .modify(_.addEnv("POSTGRES_USER", "postgres"))
            .modify(_.addEnv("POSTGRES_PASSWORD", "1234"))
            .modify(_.addEnv("POSTGRES_DB", "postgres"))
            .modify(_.addEnv("POSTGRES_HOST_AUTH_METHOD", "trust"))
            .modify(_.addExposedPort(5432))
            .modify(_.withNetwork(n))
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
  }

  def migrationContainer(migrationsPath: fs2.io.file.Path, db: Postgres, network: Network): ContainerBuilder =
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
      .modify(
        _.setStartupCheckStrategy(
          new OneShotStartupCheckStrategy().withTimeout(10.seconds.toJava)
        )
      )
      .modify(_.withNetwork(network))

  def migrate(migrationsPath: fs2.io.file.Path, db: Fixture[Postgres], network: Fixture[Network]): Suite[Unit] =
    fixture {
      for {
        db <- db
        network <- network
      } yield Stream.resource {
        migrationContainer(migrationsPath, db, network).start[IO]
      }
    }.void

  def migratedPostgres(
    migrationsPath: fs2.io.file.Path
  ): Suite[Fixture[Postgres]] =
    for {
      nw <- network
      db <- postgres(nw)
      _ <- migrate(migrationsPath, db, nw)
    } yield db

  final case class Redpanda(rpk: GenericContainer[?]) {
    def host = rpk.getHost()
    def port = rpk.getMappedPort(9092).toInt
  }
  def redpanda: Suite[Fixture[Redpanda]] = {
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
          .modify(
            _.withCreateContainerCmdModifier(
              _.withEntrypoint("bash", "-c", cmd)
            )
          )
          .modify(_.addExposedPort(9092))
          .start[IO]
      }
    }.map(_.map(Redpanda(_)))
  }

  final case class Sftp(sftp: GenericContainer[?]) {
    def host = sftp.getHost()
    def port = sftp.getMappedPort(22).toInt
  }
  def sftp(key: fs2.io.file.Path): Suite[Fixture[Sftp]] =
    fixture {
      Stream.resource {
        ContainerBuilder("atmoz/sftp")
          .modify(_.addExposedPort(22))
          .modify(_.setCommand("useruser:passpass:::folder,collection"))
          .modify(
            _.addFileSystemBind(
              key.absolute.toString,
              "/home/useruser/.ssh/id_rsa.pub",
              BindMode.READ_ONLY
            )
          )
          .start[IO]
          .map(Sftp(_))
      }
    }

  final case class Spice(spicedb: GenericContainer[?]) {
    def host = spicedb.getHost()
    def port = spicedb.getMappedPort(50051).toInt
  }
  def makeSpice(pg: Fixture[Postgres], network: Fixture[Network]): Suite[Fixture[Spice]] =
    fixture {
      (network, pg).mapN { (n, db) =>
        Stream.resource {
          ContainerBuilder("authzed/sapicedb:v1.25.0")
            .modify(_.addEnv("SPICEDB_GRPC_PRESHARED_KEY", "key"))
            .modify(_.addExposedPort(50051))
            .modify(_.withNetwork(n))
            .modify(
              _.withCommand(
                "serve",
                "--datastore-engine=postgres",
                s"--datastore-conn-uri=postgres://postgres:1234@${db.host}:${db.port}/postgres?sslmode=disable",
                "--datastore-conn-pool-read-max-open=4",
                "--datastore-conn-pool-read-min-open=1",
                "--datastore-conn-pool-write-max-open=4",
                "--datastore-conn-pool-write-min-open=1"
              )
            )
            .start[IO]
            .map(Spice(_))
        }
      }
    }

  def spice: Suite[Fixture[Spice]] = 
    for {
      nw <- network
      db <- postgres(nw)
      sp <- makeSpice(db, nw)
    } yield sp
}
