object ZA extends App {
  import Stream._
  import scala.collection.mutable.ListBuffer

object Z {
  // only for comfort and result checking:
  val fibs: Stream[BigInt] = {def series(i:BigInt,j:BigInt):Stream[BigInt] = i #:: series(j,i+j); series(1,0).tail.tail.tail }
  val z2i: Z => BigInt = z => (z.z.abs.toString.map(_.asDigit).reverse.zipWithIndex.map{case (v,i)=>v*fibs(i)}:\BigInt(0))(_+_)*z.z.signum

  var fmts = Map(Z("0")->List[Z](Z("0")))   //map of Fibonacci multiples table of divisors

  // get multiply table from fmts
  def mt(z: Z): List[Z] = {fmts.getOrElse(z,Nil) match {case Nil => {val e = mwv(z); fmts=fmts+(z->e); e}; case l => l}}

  // multiply weight vector
  def mwv(z: Z): List[Z] = {
    val wv = new ListBuffer[Z]; wv += z; wv += (z+z)
    var zs = "11"; val upper = z.z.abs.toString
    while ((zs.size<upper.size)) {wv += (wv.toList.last + wv.toList.reverse.tail.head); zs = "1"+zs}
    wv.toList
  }

  // get division table (division weight vector)
  def dt(dd: Z, ds: Z): List[Z] = {
    val wv = new ListBuffer[Z]; mt(ds).copyToBuffer(wv)
    var zs = ds.z.abs.toString; val upper = dd.z.abs.toString
    while ((zs.size<upper.size)) {wv += (wv.toList.last + wv.toList.reverse.tail.head); zs = "1"+zs}
    wv.toList
  }
}

case class Z(var zs: String) {
  import Z._
  require ((zs.toSet--Set('-','0','1')==Set()) && (!zs.contains("11")))

  var z: BigInt = BigInt(zs)
  override def toString = z+"Z(i:"+z2i(this)+")"
  def size = z.abs.toString.size

  //--- fa(summand1.z,summand2.z) --------------------------
  val fa: (BigInt,BigInt) => BigInt = (z1, z2) => {
    val v =z1.toString.map(_.asDigit).reverse.padTo(5,0).zipAll(z2.toString.map(_.asDigit).reverse, 0, 0)
    val arr1 = (v.map(p=>p._1+p._2):+0 reverse).toArray
    (0 to arr1.size-4) foreach {i=>     //stage1
      val a = arr1.slice(i,i+4).toList
      val b = (a:\"")(_+_) dropRight 1
      val a1 = b match {
          case "020" => List(1,0,0, a(3)+1)
          case "030" => List(1,1,0, a(3)+1)
          case "021" => List(1,1,0, a(3))
          case "012" => List(1,0,1, a(3))
          case _     => a
      }
      0 to 3 foreach {j=>arr1(j+i) = a1(j)}
    }
    val arr2 = (arr1:\"")(_+_)
      .replace("0120","1010").replace("030","111").replace("003","100").replace("020","101")
      .replace("003","100").replace("012","101").replace("021","110")
      .replace("02","10").replace("03","11")
      .reverse.toArray
    (0 to arr2.size-3) foreach {i=>     //stage2, step1
      val a = arr2.slice(i,i+3).toList
      val b = (a:\"")(_+_)
      val a1 = b match {
          case "110" => List('0','0','1')
          case _     => a
      }
      0 to 2 foreach {j=>arr2(j+i) = a1(j)}
    }
    val arr3 = (arr2:\"")(_+_).concat("0").reverse.toArray
    (0 to arr3.size-3) foreach {i=>     //stage2, step2
      val a = arr3.slice(i,i+3).toList
      val b = (a:\"")(_+_)
      val a1 = b match {
          case "011" => List('1','0','0')
          case _     => a
      }
      0 to 2 foreach {j=>arr3(j+i) = a1(j)}
    }
    BigInt((arr3:\"")(_+_))
  }

  //--- fs(minuend.z,subtrahend.z) -------------------------
  val fs: (BigInt,BigInt) => BigInt = (min,sub) => {
    val zmvr = min.toString.map(_.asDigit).reverse
    val zsvr = sub.toString.map(_.asDigit).reverse.padTo(zmvr.size,0)
    val v = zmvr.zipAll(zsvr, 0, 0).reverse
    val last = v.size-1
    val zma = zmvr.reverse.toArray; val zsa = zsvr.reverse.toArray
    for (i <- 0 to last reverse) {
      val e = zma(i)-zsa(i)
      if (e<0) {
        zma(i-1) = zma(i-1)-1
        zma(i) = 0
        val part = Z((((i to last).map(zma(_))):\"")(_+_))
        val carry = Z(("1".padTo(last-i,"0"):\"")(_+_))
        val sum = part + carry; val sums = sum.z.toString
        (1 to sum.size) foreach {j=>zma(last-sum.size+j)=sums(j-1).asDigit}
        if (zma(i-1)<0) {
          for (j <- 0 to i-1 reverse) {
            if (zma(j)<0) {
              zma(j-1) = zma(j-1)-1
              zma(j) = 0
              val part = Z((((j to last).map(zma(_))):\"")(_+_))
              val carry = Z(("1".padTo(last-j,"0"):\"")(_+_))
              val sum = part + carry; val sums = sum.z.toString
              (1 to sum.size) foreach {k=>zma(last-sum.size+k)=sums(k-1).asDigit}
            }
          }
        }
      }
      else zma(i) = e
      zsa(i) = 0
    }
    BigInt((zma:\"")(_+_))
  }

  //--- fm(multiplicand.z,multplier.z) ---------------------
  val fm: (BigInt,BigInt) => BigInt = (mc, mp) => {
    val mct = mt(Z(mc.toString))
    val mpxi = mp.toString.reverse.map(_.asDigit).zipWithIndex.filter(_._1 != 0).map(_._2)
    (mpxi:\Z("0"))((fi,sum)=>sum+mct(fi)).z
  }

  //--- fd(dividend.z,divisor.z) ---------------------------
  val fd: (BigInt,BigInt) => BigInt = (dd, ds) => {
    val dst = dt(Z(dd.toString),Z(ds.toString)).reverse
    var diff = Z(dd.toString)
    val zd = ListBuffer[String]()
    (0 to dst.size-1) foreach {i=>
      if (dst(i)>diff) zd+="0" else {diff = diff-dst(i); zd+="1"}
    }
    BigInt(zd.mkString)
  }

  val fasig: (Z, Z) => Int = (z1, z2) => if (z1.z.abs>z2.z.abs) z1.z.signum else z2.z.signum
  val fssig: (Z, Z) => Int = (z1, z2) =>
    if ((z1.z.abs>z2.z.abs && z1.z.signum>0)||(z1.z.abs<z2.z.abs && z1.z.signum<0)) 1 else -1

  def +(that: Z): Z =
    if (this==Z("0")) that
    else if (that==Z("0")) this
    else if (this.z.signum == that.z.signum) Z((fa(this.z.abs.max(that.z.abs),this.z.abs.min(that.z.abs))*this.z.signum).toString)
    else if (this.z.abs == that.z.abs) Z("0")
    else Z((fs(this.z.abs.max(that.z.abs),this.z.abs.min(that.z.abs))*fasig(this, that)).toString)

  def ++ : Z = {val za = this + Z("1"); this.zs = za.zs; this.z = za.z; this}

  def -(that: Z): Z =
    if (this==Z("0")) Z((that.z*(-1)).toString)
    else if (that==Z("0")) this
    else if (this.z.signum != that.z.signum) Z((fa(this.z.abs.max(that.z.abs),this.z.abs.min(that.z.abs))*this.z.signum).toString)
    else if (this.z.abs == that.z.abs) Z("0")
    else Z((fs(this.z.abs.max(that.z.abs),this.z.abs.min(that.z.abs))*fssig(this, that)).toString)

  def -- : Z = {val zs = this - Z("1"); this.zs = zs.zs; this.z = zs.z; this}

  def * (that: Z): Z =
    if (this==Z("0")||that==Z("0")) Z("0")
    else if (this==Z("1")) that
    else if (that==Z("1")) this
    else Z((fm(this.z.abs.max(that.z.abs),this.z.abs.min(that.z.abs))*this.z.signum*that.z.signum).toString)

  def / (that: Z): Option[Z] =
    if (that==Z("0")) None
    else if (this==Z("0")) Some(Z("0"))
    else if (that==Z("1")) Some(Z("1"))
    else if (this.z.abs < that.z.abs) Some(Z("0"))
    else if (this.z == that.z) Some(Z("1"))
    else Some(Z((fd(this.z.abs.max(that.z.abs),this.z.abs.min(that.z.abs))*this.z.signum*that.z.signum).toString))

  def % (that: Z): Option[Z] =
    if (that==Z("0")) None
    else if (this==Z("0")) Some(Z("0"))
    else if (that==Z("1")) Some(Z("0"))
    else if (this.z.abs < that.z.abs) Some(this)
    else if (this.z == that.z) Some(Z("0") )
    else this/that match {case None => None; case Some(z) => Some(this-z*that)}

  def <  (that: Z): Boolean = this.z <  that.z
  def <= (that: Z): Boolean = this.z <= that.z
  def >  (that: Z): Boolean = this.z >  that.z
  def >= (that: Z): Boolean = this.z >= that.z

}

val elapsed: (=> Unit) => Long = f => {val s = System.currentTimeMillis; f; (System.currentTimeMillis - s)/1000}

val add:      (Z,Z) => Z = (z1,z2) => z1+z2
val subtract: (Z,Z) => Z = (z1,z2) => z1-z2
val multiply: (Z,Z) => Z = (z1,z2) => z1*z2
val divide:   (Z,Z) => Option[Z] = (z1,z2) => z1/z2
val modulo:   (Z,Z) => Option[Z] = (z1,z2) => z1%z2

val ops = Map(("+",add),("-",subtract),("*",multiply),("/",divide),("%",modulo))

val calcs = List(
  (Z("101"),"+",Z("10100"))
, (Z("101"),"-",Z("10100"))
, (Z("101"),"*",Z("10100"))
, (Z("101"),"/",Z("10100"))
, (Z("-1010101"),"+",Z("10100"))
, (Z("-1010101"),"-",Z("10100"))
, (Z("-1010101"),"*",Z("10100"))
, (Z("-1010101"),"/",Z("10100"))
, (Z("1000101010"),"+",Z("10101010"))
, (Z("1000101010"),"-",Z("10101010"))
, (Z("1000101010"),"*",Z("10101010"))
, (Z("1000101010"),"/",Z("10101010"))
, (Z("10100"),"+",Z("1010"))
, (Z("100101"),"-",Z("100"))
, (Z("1010101010101010101"),"+",Z("-1010101010101"))
, (Z("1010101010101010101"),"-",Z("-1010101010101"))
, (Z("1010101010101010101"),"*",Z("-1010101010101"))
, (Z("1010101010101010101"),"/",Z("-1010101010101"))
, (Z("1010101010101010101"),"%",Z("-1010101010101"))
, (Z("1010101010101010101"),"+",Z("101010101010101"))
, (Z("1010101010101010101"),"-",Z("101010101010101"))
, (Z("1010101010101010101"),"*",Z("101010101010101"))
, (Z("1010101010101010101"),"/",Z("101010101010101"))
, (Z("1010101010101010101"),"%",Z("101010101010101"))
, (Z("10101010101010101010"),"+",Z("1010101010101010"))
, (Z("10101010101010101010"),"-",Z("1010101010101010"))
, (Z("10101010101010101010"),"*",Z("1010101010101010"))
, (Z("10101010101010101010"),"/",Z("1010101010101010"))
, (Z("10101010101010101010"),"%",Z("1010101010101010"))
, (Z("1010"),"%",Z("10"))
, (Z("1010"),"%",Z("-10"))
, (Z("-1010"),"%",Z("10"))
, (Z("-1010"),"%",Z("-10"))
, (Z("100"),"/",Z("0"))
, (Z("100"),"%",Z("0"))
)

// just for result checking:
import Z._
val iadd: (BigInt,BigInt) => BigInt = (a,b) => a+b
val isub: (BigInt,BigInt) => BigInt = (a,b) => a-b
val imul: (BigInt,BigInt) => BigInt = (a,b) => a*b
val idiv: (BigInt,BigInt) => Option[BigInt] = (a,b) => if (b==0) None else Some(a/b)
val imod: (BigInt,BigInt) => Option[BigInt] = (a,b) => if (b==0) None else Some(a%b)
val iops = Map(("+",iadd),("-",isub),("*",imul),("/",idiv),("%",imod))

println("elapsed time: "+elapsed{
    calcs foreach {case (op1,op,op2) => println(op1+" "+op+" "+op2+" = "
      +{(ops(op))(op1,op2) match {case None => None; case Some(z) => z; case z => z}}
        .ensuring{x=>(iops(op))(z2i(op1),z2i(op2)) match {case None => None == x; case Some(i) => i == z2i(x.asInstanceOf[Z]); case i => i == z2i(x.asInstanceOf[Z])}})}
  }+" sec"
)

}
