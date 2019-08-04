sealed trait Nat
object Nat {

  case object Zero extends Nat

  type Zero = Zero.type
  type _0 = Zero
  type _1 = Succ[_0]
  type _2 = Succ[_1]
  type _3 = Succ[_2]
  type _4 = Succ[_3]
  type _5 = Succ[_4]
  type _6 = Succ[_5]
  type _7 = Succ[_6]
  type _8 = Succ[_7]
  type _9 = Succ[_8]
  type _10 = Succ[_9]
  type _11 = Succ[_10]
  type _12 = Succ[_11]
  type _13 = Succ[_12]
  type _14 = Succ[_13]
  type _15 = Succ[_14]

  case class Succ[N <: Nat]() extends Nat

  trait AsInt[N <: Nat] { def value: Int }
  object AsInt {

    def apply[N <: Nat: AsInt]: AsInt[N] = implicitly[AsInt[N]]

    implicit val zeroToInt: AsInt[Zero] = new AsInt[Zero] {
      def value: Int = 0
    }

    implicit def succToInt[N <: Nat: AsInt]: AsInt[Succ[N]] =
      new AsInt[Succ[N]] { def value: Int = implicitly[AsInt[N]].value + 1 }

  }

  trait Diff[A <: Nat, B <: Nat] { type Out <: Nat }
  object Diff {

    type Aux[A <: Nat, B <: Nat, C <: Nat] = Diff[A, B] { type Out = C }

    def apply[A <: Nat, B <: Nat](implicit D: Diff[A, B]): Aux[A, B, D.Out] =
      D

    implicit def minusZero[A <: Nat]: Aux[A, Zero, A] =
      new Diff[A, Zero] { override type Out = A }

    implicit def minus[A <: Nat, B <: Nat, C <: Nat](
      implicit D: Aux[A, B, C]
    ): Aux[Succ[A], Succ[B], C] = new Diff[Succ[A], Succ[B]] {
      override type Out = C
    }
  }

  sealed trait Divisible[D <: Nat, N <: Nat]
  object Divisible {

    def apply[D <: Nat, N <: Nat](
      implicit D: Divisible[D, N]
    ): Divisible[D, N] = D

    implicit def zeroDiv[D <: Succ[_]]: Divisible[D, Zero] =
      new Divisible[D, Zero] {}

    implicit def succDiv[D <: Succ[_], N <: Nat, Diff <: Nat](
      implicit diff: Diff.Aux[N, D, Diff],
      D: Divisible[D, Diff]
    ): Divisible[D, N] = new Divisible[D, N] {}
  }

  sealed trait IsEven[N <: Nat]
  case class Yes[N <: Nat]() extends IsEven[N]
  case class No[N <: Nat]() extends IsEven[N]

  trait LowPriority {
    implicit def isOdd[N <: Nat]: IsEven[N] = No[N]
  }
  trait HighPriority extends LowPriority {
    implicit def isEven[N <: Nat](implicit D: Divisible[_2, N]): IsEven[N] =
      Yes[N]
  }
  object IsEven extends HighPriority

  case class FizzBuzzValue[N <: Nat](message: String)

  type Divisible3[N <: Nat] = Divisible[_3, N]
  type Divisible5[N <: Nat] = Divisible[_5, N]

  trait Fallback {

    implicit def fizzBuzzValueNum[N <: Nat: AsInt]: FizzBuzzValue[N] =
      FizzBuzzValue(AsInt[N].value.toString)
  }
  trait Divisible3Or5 extends Fallback {

    implicit def divisible3[N <: Nat: Divisible3]: FizzBuzzValue[N] =
      FizzBuzzValue("Fizz")

    implicit def divisible5[N <: Nat: Divisible5]: FizzBuzzValue[N] =
      FizzBuzzValue("Buzz")
  }
  trait Divisible3And5 extends Divisible3Or5 {

    implicit def divisible3And5[N <: Nat: Divisible3: Divisible5]
      : FizzBuzzValue[N] = FizzBuzzValue("FizzBuzz")
  }
  object FizzBuzzValue extends Divisible3And5

  case class Range[Exec[_ <: Nat], From <: Nat, To <: Nat](
    prev: Option[Range[Exec, _, _]],
    exec: Exec[_],
  )
  object Range {

    implicit def one[Exec[_ <: Nat], N <: Nat](
      implicit Exec: Exec[N]
    ): Range[Exec, N, N] = Range(None, Exec)

    implicit def range[Exec[_ <: Nat], From <: Nat, To <: Nat](
      implicit Prev: Range[Exec, From, To],
      Exec: Exec[Succ[To]]
    ): Range[Exec, From, Succ[To]] = Range(Some(Prev), Exec)
  }
}

object FizzBuzz {
  import Nat._

  case class PrintFizzBuzzMessage[N <: Nat](fizzBuzzValue: FizzBuzzValue[N]) {

    println(fizzBuzzValue.message)
  }
  object PrintFizzBuzzMessage {

    implicit def print[N <: Nat: FizzBuzzValue]: PrintFizzBuzzMessage[N] =
      PrintFizzBuzzMessage(implicitly[FizzBuzzValue[N]])
  }

  def main(args: Array[String]): Unit = {

    implicitly[Range[PrintFizzBuzzMessage, _1, _15]]
  }
}
