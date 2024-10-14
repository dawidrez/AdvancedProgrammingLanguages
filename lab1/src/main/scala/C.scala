package c;

class C(val re: Double, val im: Double) {
  
    def this(re: Double) = {
      this(re, 0.0) 
    }
    def +(that: C): C = C(this.re + that.re, this.im + that.im)
    def -(that: C): C = C(this.re - that.re, this.im - that.im)
    def *(that: C): C = C(this.re * that.re - this.im * that.im, this.re * that.im + this.im * that.re)

   def /(that: C): C = {
      val denom = that.re * that.re + that.im * that.im
      if (denom == 0){
        throw new IllegalArgumentException("Divisor cannot be zero.")
      }
     C(
      (this.re * that.re + this.im * that.im) / denom,
      (this.im * that.re - this.re * that.im) / denom
    )
  }
    def +(x: Double): C = C(this.re + x, this.im)
    def -(x: Double): C = C(this.re - x, this.im)
    def *(x: Double): C = C(this.re * x, this.im * x)

  def /(x: Double): C = {
    if (x  == 0){
        throw new IllegalArgumentException("Divisor cannot be zero.")
    }
    C(
      (this.re /x),
      (this.im /x)
    )
  }
    override def toString: String = {
    if (this.im > 0){
        s"${this.re} + ${this.im}i"
    }
    else if (this.im < 0){
        s"${this.re} - ${(this.im).abs}i"
    }
    else{
        s"${this.re}"
    }
  }
  def >(that: C): Boolean={
    this.modulus > that.modulus
  }
   def <(that: C): Boolean={
    this.modulus < that.modulus
  }
   def ==(that: C): Boolean={
    this.modulus == that.modulus
  }
   def !=(that: C): Boolean={
    this.modulus != that.modulus
  }

   def <=(that: C): Boolean={
    this.modulus <= that.modulus
  }
   def >=(that: C): Boolean={
    this.modulus >= that.modulus
  }

    def modulus: Double ={
        Math.sqrt(this.re*this.re+this.im*this.im)
    }

}