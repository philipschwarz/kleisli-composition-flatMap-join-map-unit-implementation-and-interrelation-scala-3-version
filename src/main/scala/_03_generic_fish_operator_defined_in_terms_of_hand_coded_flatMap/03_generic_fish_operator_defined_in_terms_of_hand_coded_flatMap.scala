package _03_generic_fish_operator_defined_in_terms_of_hand_coded_flatMap

import _00_domain.*

trait Monad[F[_]]:
  def unit[A](a: => A): F[A]
  extension [A](fa: F[A])
    def flatMap[B](f: A => F[B]): F[B]

extension [A,B,F[_]: Monad](f: A => F[B])
  def >=>[C](g: B => F[C]): A => F[C] =
    a => f(a).flatMap(g)

given Monad[Option] with
  def unit[A](a: => A): Option[A] = Some(a)
  extension [A](fa: Option[A])
    def flatMap[B](f: A => Option[B]): Option[B] =
      fa match
        case Some(a) => f(a)
        case None => None

given Monad[List] with
  def unit[A](a: => A): List[A] = List(a)
  extension [A](fa: List[A])
    def flatMap[B](f: A => List[B]): List[B] =
      fa.foldRight(List.empty[B]):
        (a,bs) => f(a) ++ bs

val carInsurance: Person => Option[Insurance] =
  car >=> insurance

val toCharsAscii: String => List[Char] =
  toChars >=> toAscii

@main def main03: Unit =

  assert(carInsurance(nonDriver).isEmpty)
  assert(carInsurance(uninsured).isEmpty)
  assert(carInsurance(insured).contains(Insurance("Acme")))
  
  assert(toCharsAscii("AB") == List('6','5','6','6'))