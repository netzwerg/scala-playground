package ch.netzwerg.euler

object ProjectEuler001 extends App {

  val result = ((1 until 1000) filter (x => (x % 5 == 0 || x % 3 == 0))).toList.sum

  println("Problem 1: " + result)

}

object ProjectEuler002 extends App {

  val fibs: Stream[Int] = 0 #:: 1 #:: fibs.zip(fibs.tail).map {
    n => n._1 + n._2
  }
  val result = (fibs takeWhile (_ < 4000000) filter (_ % 2 == 0)).toList.sum

  println("Problem 2: " + result)
}

object ProjectEuler003 extends App {

  val primes: Stream[Int] = 2 #:: Stream.from(3).filter(
    i => primes.takeWhile(j => j * j <= i).forall(i % _ > 0)
  )

  // recursively factoring number 'n' into given primes
  def factor(acc: Stream[BigInt], n: BigInt, primes: Stream[Int]): Stream[BigInt] = {
    if (n == 1) acc
    else if (n % primes.head == 0) factor(primes.head #:: acc, n / primes.head, primes)
    else factor(acc, n, primes.tail)
  }

  val result = factor(Stream.empty, BigInt(600851475143L), primes).head

  println("Problem 3: " + result)

}

object ProjectEuler004 extends App {

  val result = (for {
    i <- (100 to 999)
    j <- (100 to 999)
  } yield (i * j)).filter(x => x.toString == x.toString.reverse).max

  println("Problem 4: " + result)

}

object ProjectEuler005 extends App {

  val result = Range(20, Int.MaxValue).find(i => (2 to 20).forall(i % _ == 0)).get

  println("Problem 5: " + result)

}

object ProjectEuler006 extends App {

  val sum = (1 to 100).sum
  val result = ((1 to 100).map(x => x * x).sum - (sum * sum)).abs

  println("Problem 6: " + result)

}

object ProjectEuler007 extends App {

  val primes: Stream[Int] = 2 #:: Stream.from(3).filter(
    i => primes.takeWhile(j => j * j <= i).forall(i % _ > 0)
  )

  val result = primes(10000)

  println("Problem 7: " + result)

}

object ProjectEuler008 extends App {

  val number = "73167176531330624919225119674426574742355349194934" +
    "96983520312774506326239578318016984801869478851843" +
    "85861560789112949495459501737958331952853208805511" +
    "12540698747158523863050715693290963295227443043557" +
    "66896648950445244523161731856403098711121722383113" +
    "62229893423380308135336276614282806444486645238749" +
    "30358907296290491560440772390713810515859307960866" +
    "70172427121883998797908792274921901699720888093776" +
    "65727333001053367881220235421809751254540594752243" +
    "52584907711670556013604839586446706324415722155397" +
    "53697817977846174064955149290862569321978468622482" +
    "83972241375657056057490261407972968652414535100474" +
    "82166370484403199890008895243450658541227588666881" +
    "16427171479924442928230863465674813919123162824586" +
    "17866458359124566529476545682848912883142607690042" +
    "24219022671055626321111109370544217506941658960408" +
    "07198403850962455444362981230987879927244284909188" +
    "84580156166097919133875499200524063689912560717606" +
    "05886116467109405077541002256983155200055935729725" +
    "71636269561882670428252483600823257530420752963450"

  def digitProduct(s: String): Int = {
    s.map(_.asDigit).foldLeft(1)(_ * _)
  }

  val result = number.sliding(5).map(digitProduct).max

  println("Problem 8: " + result)

}
