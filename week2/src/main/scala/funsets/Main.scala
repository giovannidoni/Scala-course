package funsets

object Main extends App {
  import FunSets._
  println(contains(singletonSet(1), 1))
  println(contains(singletonSet(2), 1))

  val  myUnion: Set = union(union(singletonSet(1), singletonSet(2)), singletonSet(4))
  printSet(myUnion)

  val  myInt: Set = intersect(myUnion, singletonSet(1))
  printSet(myInt)

  val  myDiff: Set = diff(myUnion, singletonSet(1))
  printSet(myDiff)

  val myFilter: Set = filter(myUnion, x => x>2)
  printSet(myFilter)

  println(forall(myUnion, x => x>0))

  printSet(myUnion)
  println(exists(myUnion, x => x>=2))

  printSet(map(myUnion, x => x*x))

  val n1 = new Imaginary(-1, -2)
  val n2 = new Imaginary(2, -3)
//  println("First number " + n1.toString)
//  println("Second number " + n2.toString)
//  println("Negative number " + -n2)
//
//  println("Sum of the two is " + (n1 + n2).toString)
//  println(n1 > n2)
//  println("Modulus " + n2.mod)
//  println("Product is " + (n1*n2).toString)
//  println("Modulus of the product " + (n1*n2).%)
//  println("Modulus of first number " + n2.%)
//  println("Modulus of second number " + n1.%)

}

class Imaginary(x: Int, y: Int) {
  def real = x
  def imm = y

  def mod = this.%

  def abs(x: Int) = if (x>0) x else -x

  override def toString = {
    if (imm > 0) real + " + " + imm + "i"
    else real + " - " + abs(imm) + "i"
  }

  def sum(f: Int => Int)(a: Int, b:Int) = f(a) + f(b)

  def + (that: Imaginary) =
    new Imaginary(real + that.real, imm + that.imm)

  def unary_- = new Imaginary(-real, -imm)

  def > (that: Imaginary) = this.% > that.%

  def % : Double = sum(x => x*x)(real, imm)

  def *  (that: Imaginary) =
    new Imaginary(real*that.real - imm*that.imm, real*that.imm + imm*that.real)

  }
