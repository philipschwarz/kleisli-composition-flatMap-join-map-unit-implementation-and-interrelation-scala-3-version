package _01_a_simple_example_of_hand_coding_kleisli_composition_for_option_and_list

import _00_domain.{toAscii, toChars}

extension [A, B](f: A => List[B])
  def >=>[C](g: B => List[C]): A => List[C] =
    a => f(a).foldRight(List.empty[C]):
      (b, cs) => g(b) ++ cs

val toCharsAscii: String => List[Char] =
  toChars >=> toAscii

@main def main_01_2: Unit =

  assert(toCharsAscii("AB") == List('6','5','6','6'))