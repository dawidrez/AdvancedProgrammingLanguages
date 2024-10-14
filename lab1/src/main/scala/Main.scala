import c.C

@main
def Main:Unit={
  println("hello enter a number")
  val number = io.StdIn.readInt()
  exercise(number)
}

@main
def Main2:Unit={
  val number_1 = C(2,2)
  val number_2 = C(4)
  val number_3 = C(2, -4)
  val number_4 = C(0, 0)
  println(s"${number_1} + ${number_2} = ${number_1+ number_2}")
  println(s"${number_1} + ${number_3} = ${number_1+ number_3}")
   println(s"${number_1} - ${number_2} = ${number_1- number_2}")
  println(s"${number_1} - ${number_3} = ${number_1- number_3}")
   println(s"${number_1} * ${number_2} = ${number_1* number_2}")
  println(s"${number_1} * ${number_3} = ${number_1* number_3}")
   println(s"${number_1} / ${number_2} = ${number_1/ number_2}")
     println(s"${number_1} / ${number_3} = ${number_1/ number_3}")
 
  try {
    println(s"${number_1} / ${0} = ${number_1/ 0}")
} catch {
    case e: IllegalArgumentException => println(e)
}
 try {
    println(s"${number_4} / ${number_4} = ${number_4/ number_4}")
} catch {
    case e: IllegalArgumentException => println(e)
}
   println(s"${number_1} > ${number_3} = ${number_1 > number_3}")
  println(s"${number_1} < ${number_3} = ${number_1 < number_3}")
   println(s"${number_1} == ${number_3} = ${number_1 == number_3}")
  println(s"${number_1} != ${number_3} = ${number_1 != number_3}")
   println(s"${number_1} >= ${number_3} = ${number_1 >= number_3}")
  println(s"${number_1} <= ${number_3} = ${number_1 <= number_3}")
}


def evenNumbersLessThanX(x: Int): List[Int] = {
  (3 until x+1).filter(n => n % 2 == 0).toList
}

def exercise(x: Int): Unit = {
  val numbers = evenNumbersLessThanX(x)
  for (n <- numbers) {
    println(n)
    val pairs = findPairs(getSmallerPrimeNumbers(n), n)
    for(pair <- pairs) println(pair)
  }
}

def findDividers(x:Int): List[Int]={
  val halfX = x/2
  (2 until halfX).filter(n => x%n==0).toList
}
def isPrime(x: Int): Boolean ={
  findDividers(x).isEmpty 

}

def getSmallerPrimeNumbers(x:Int): List[Int]={
  (2 until x).filter(n => isPrime(n)).toList
}

def findPairs(list: List[Int], x: Int): List[(Int, Int)] = {
list.combinations(2).filter { case List(a, b) => a + b == x }.map { case List(a, b) => (a, b) }.toList
}