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
  } yield (i * j)).filter(x => x.toString == x.toString.reverse).sorted.last

  println("Problem 4: " + result)

}

object ProjectEuler005 extends App {

    val result = Range(20, Int.MaxValue).find(i => (2 to 20).forall(i % _ == 0)).get

    println("Problem 5: " + result)

}

object ProjectEuler007 extends App {

  val primes: Stream[Int] = 2 #:: Stream.from(3).filter(
    i => primes.takeWhile(j => j * j <= i).forall(i % _ > 0)
  )

  val result = primes(10000)

  println("Problem 7: " + result)

}
