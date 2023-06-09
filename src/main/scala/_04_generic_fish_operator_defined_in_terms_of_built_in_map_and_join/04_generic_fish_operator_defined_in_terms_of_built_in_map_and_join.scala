package _04_generic_fish_operator_defined_in_terms_of_built_in_map_and_join

import _00_domain.*

trait Functor[F[_]]:
  extension [A](fa: F[A])
    def map[B](f: A => B): F[B]

trait Monad[F[_]] extends Functor[F]:
  def unit[A](a: => A): F[A]
  extension [A](ffa: F[F[A]])
    def join: F[A]

extension [A,B,F[_]: Monad] (f: A => F[B])
  def >=>[C](g: B => F[C]): A => F[C] =
    a => f(a).map(g).join

given Monad[Option] with
  def unit[A](a: => A): Option[A] = Some(a)
  extension [A](fa: Option[A])
    def map[B](f: A => B): Option[B] = fa.map(f)
  extension [A](ffa: Option[Option[A]])
    def join: Option[A] = ffa.flatten

given Monad[List] with
  def unit[A](a: => A): List[A] = List(a)
  extension [A](fa: List[A])
    def map[B](f: A => B): List[B] = fa.map(f)
  extension[A] (ffa: List[List[A]])
    def join: List[A] = ffa.flatten

val carInsurance: Person => Option[Insurance] =
  car >=> insurance

val toCharsAscii: String => List[Char] =
  toChars >=> toAscii

@main def main_04: Unit =

  assert(carInsurance(nonDriver).isEmpty)
  assert(carInsurance(uninsured).isEmpty)
  assert(carInsurance(insured).contains(Insurance("Acme")))

  assert(toCharsAscii("AB") == List('6','5','6','6'))