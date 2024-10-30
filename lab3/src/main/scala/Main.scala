import scala.annotation.tailrec


type MSet[A] = A => Int
@main
def mainProg2: Unit = {
  //println(divide(List(1, 3, 5, 6, 7)))
  //print(merge(List(1 ,3, 5, 8), List(2, 4, 6, 8, 10, 12))((m, n) => m < n))
  //print(compress(List('a','a','b','c','c','c','d','d','c')))
  //print(isSub(List('b', 'o', 'c', 'i', 'a', 'n'), List('a', 'b', 'c') ))
val result = compute(List(Some(1), None, Some(2), None, Some(3), Some(4)))(_ + 0)(_ + _)
println(result)

}


def divide[A](list: List[A]): (List[A], List[A]) ={

  @tailrec
  def helper(list:List[A], index: Int, l1: List[A], l2: List[A]): (List[A], List[A]) ={
    list match {
      case List() => (l1.reverse, l2.reverse)
      case lH::lT if (index%2==0) => helper(lT, index+1, lH::l1, l2)
      case lH::lT if (index%2==1) => helper(lT, index+1, l1, lH::l2)
    }
  }
  helper(list, 0, List(), List())

} 


def merge[A](a: List[A], b: List[A])(leq: (A, A) => Boolean): List[A] ={

  @tailrec
  def helper[A](a: List[A], b: List[A], c:List[A])(leq: (A, A) => Boolean): List[A] ={
    (a,b) match{
      case (List(), List()) => c.reverse
      case (List(), lH::lT) => helper(a, lT, lH::c)(leq)
      case (lH::lT, List()) => helper(lT, b, lH::c)(leq)
      case (lH1::lT1, lH2::lT2) if(leq(lH1, lH2)) => helper(lT1, b, lH1::c)(leq) 
      case (lH1::lT1, lH2::lT2) if(!leq(lH1, lH2)) => helper(a, lT2, lH2::c)(leq) 
    }
  
  }
  helper(a,b, List())(leq)
}

def compress[A](l: List[A]):List[(A, Int)]={
  @tailrec
  def helper[A](l: List[A], acc:List[(A, Int)]):List[(A, Int)]={
    (l,acc) match {
      case (List(), _) => acc.reverse
      case (lH::lT, List()) => helper(lT, (lH,1)::acc)
      case (lH1::lT1, lH2::lT2) if (lH1==lH2(0)) => {
        val counter = lH2(1)+1
        helper(lT1, (lH1,counter)::lT2)
    }
     case (lH1::lT1, lH2::lT2) if (lH1!=lH2(0)) => {
        helper(lT1, (lH1,1)::acc)
    
  }
  }
  }
  helper(l, List())
}

def isSub[A](l: List[A], lSub: List[A]): Boolean={

@tailrec
def helper[A](l: List[A], partL: List[A], lSub: List[A]): Boolean={
  (partL,lSub) match {
      case (List(), _) => false
      case (_, List()) => true
      case (lH1::lT1, lH2::lT2) if (lH1==lH2) => helper(l, l, lT2)
      case (lH1::lT1, lH2::lT2) if (lH1!=lH2) => helper(l, lT1, lSub)
  }
}
helper(l,l, lSub)
}


def compute[A, B](l: List[Option[A]])(op1: A => B)(op2: (A, B) => B): Option[B] ={

  @annotation.tailrec
  def helper(remaining: List[Option[A]], acc: Option[B]): Option[B] = remaining match {
    case List() => acc
    case Some(value) :: tail => 
      acc match {
        case None => helper(tail, Some(op1(value)))
        case Some(accValue) => helper(tail, Some(op2(value, accValue)))
      }
    case None :: tail => helper(tail, acc)
  }

  helper(l, None)
}

def compose[A, B, C](f: A => B)(g: B => C): A => C = {
  a => g(f(a))
}

def prod[A, B, C, D](f: A => C, g: B => D): (A, B) => (C, D) = {
  (a,b)=>(f(a), g(b))
}

def lift[A, B, T](op: (T, T) => T)(f: A => T, g: B => T): (A, B) => T = 
  (a: A, b: B) => op(f(a), g(b))
