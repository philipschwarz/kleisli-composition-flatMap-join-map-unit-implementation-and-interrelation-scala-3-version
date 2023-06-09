package _01_a_simple_example_of_hand_coding_kleisli_composition_for_option

import _00_domain.*

extension [A,B](f: A => Option[B])
  def >=>[C](g: B => Option[C]): A => Option[C] =
    a => f(a) match
      case Some(b) => g(b)
      case None => None

val carInsurance: Person => Option[Insurance] =
  car >=> insurance

@main def main_01_1: Unit =

  assert( carInsurance(nonDriver).isEmpty)
  assert( carInsurance(uninsured).isEmpty )
  assert( carInsurance(insured).contains(Insurance("Acme")) )