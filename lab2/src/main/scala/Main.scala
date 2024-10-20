import scala.annotation.tailrec

@main
def mainProg: Unit = {
  //println(reverse("kot"))
  //println(isPrime(10))   
  //println(binToDec(1001))
  println(value(8)) 
}

@tailrec
def reverse(str: String, newTail: String = ""): String ={
  if(str== "") newTail
  else reverse(str.tail, str.head+newTail)
}


def isPrime(n:Integer): Boolean = {
  @tailrec
  def findDividers(n: Integer, m: Integer=1, dividers: List[Int]): List[Int] ={
    if (m == n) dividers :+ n
    else {
      if (n%m==0){
          findDividers(n, m+1, dividers :+ m)
      }
      else findDividers(n, m+1, dividers)
    }
  }
  if (n < 2) {
    false
  }
  else{
    var emptyList  = List[Int]()
    val dividers = findDividers(n,1, emptyList)
    println(dividers)
    if (dividers.length >2) false
    else true

  } 
}

def binToDec(n: Integer): Int={
  @tailrec
  def helper(n: Int, result: Int=0, power:Int=0): Int= {
    if (n==0) result
    else{
      val addition = scala.math.pow(2, power).toInt * (n%2)
      val newResult = addition + result
      helper(n/10, newResult, power+1)
    }
  }
  helper(n)
}

def value(n: Int): Int={
  @tailrec
  def helper(n:Int, value: Int= 0, value2: Int=0):Int={
    if (n==1) value + 2 + value2
    else if(n==2) value +1 +value2
    else helper(n-1, value) + helper(n-2, value)
  }
  helper(n)
}

