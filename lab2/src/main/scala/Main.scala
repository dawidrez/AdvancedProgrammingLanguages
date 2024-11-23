import scala.annotation.tailrec

@main
def mainProg: Unit = {
  println(reverse("kot"))
  println(isPrime(10))   
  println(binToDec(1001))
  println(value(8))
  print(isOrdered(Array(1, 3, 3, 6, 8), _ <= _) )
 val tab1 = Array(-1, 3, 2, -8, 5)
val tab2 = Array(-3, 3, 3, 0, -4, 5)

val result = worth(tab1, tab2)(
  (x: Int, y: Int) => x < y  
)(
  (x: Int, y: Int) => x + y
)

println(result)
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
  def helper(n:Int, value: Int, value2: Int):Int={
    if (n==0) value
    else helper(n-1, value2, value+value2)
  }
  helper(n, 2, 1)
}

def isOrdered(arr: Array[Int], mlr:(Int, Int) => Boolean): Boolean={
  @tailrec
  def helper(arr: Array[Int], mlr:(Int, Int) => Boolean): Boolean={
    if (arr.length < 2) true
    else {
      if(!mlr(arr(0), arr(1))){
        false
      }
      helper(arr.tail, mlr)
    }
  }
  helper(arr, mlr)
}

def worth(tab1: Array[Int], tab2: Array[Int])(pred: (Int, Int) => Boolean)(op: (Int, Int) => Int): Option[Int] ={
  @tailrec
  def helper(tab1: Array[Int], tab2: Array[Int]): Option[Int]= {
    if (tab1.length==0 || tab2.length==0) None
    else if (pred(tab1(0), tab2(0)) ){
      Some(op(tab1(0), tab2(0))) 
    }
    else helper(tab1.tail, tab2.tail)
  } 
  helper(tab1, tab2)
}

