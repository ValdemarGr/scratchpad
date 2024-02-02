package scratchpad

import cats.free._
import org.testcontainers.containers.{GenericContainer, Container}
import cats.data._
import cats.implicits._
import cats.effect._

final case class ContainerBuilder(
    image: String,
    make: GenericContainer[?] => GenericContainer[?] = identity
) {
  def andThen(f: GenericContainer[?] => GenericContainer[?]) =
    ContainerBuilder(image, make andThen f)

  def modify(f: GenericContainer[?] => Unit) =
    andThen(x => { f(x); x })

  def start[F[_]](implicit F: Sync[F]): Resource[F, GenericContainer[?]] =
    Resource.make(
      F.delay(make(new GenericContainer(image)))
        .flatTap(x => F.blocking(x.start()))
    )(x => F.blocking(x.stop()))
}
