package example

import fp._

import fp.instances._
import fp.syntax._

object Program {
  type Log = List[String]

  def const[A,B]: A => B => A = (a:A) => (b:B) => a

  val checkForAsteroidHeadingForEarth: Const[Log, Boolean] =
    Const(List("Check Deep Space Radar Telemetry"))

  val callBruceWillis: Const[Log, Unit] =
    Const(List("Call Bruce Willis"))

  val sendBruceWillisIntoSpace: Const[Log, Unit] =
    Const(List("Launch Bruce Willis into Space"))

  val keepLooking: Const[Log, Unit] = Const(List("The world is safe"))

  def program[F[_]](nt: NT[Const[Log, ?], F[?]])(implicit F: Selective[F]): F[Unit] =
    Selective.ifS[F, Unit](
      nt(checkForAsteroidHeadingForEarth),
      nt(callBruceWillis) |*>| nt(sendBruceWillisIntoSpace),
      nt(keepLooking)
    )

  val over = program[Over[Log, ?]](Over.fromConst)

  val under = program[Under[Log, ?]](Under.fromConst)

  def main(args: Array[String]): Unit = {
    println(s"""
               |
               |The program will definitely perform all these actions:
               |
               | ${under.value.mkString("\n ")}
               |
               |
               |The program could perform all of these actions:
               |
               | ${over.value.mkString("\n ")}
               |
             """.stripMargin)


  }

}