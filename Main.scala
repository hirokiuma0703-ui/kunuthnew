object Main {
  def main(args: Array[String]): Unit = {
    // 2048ビット級の巨大データ
    val u = BigInt("99999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999")
    val v = BigInt("1000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000009")

    val warmUpIters = 500
    val measureIters = 1000

    println("--- クヌースのアルゴリズム (16bit) 計測開始 ---")
    for (_ <- 1 to warmUpIters) divideKnuth16(u, v)

    val start = System.nanoTime()
    for (_ <- 1 to measureIters) divideKnuth16(u, v)
    val end = System.nanoTime()

    val average = (end - start).toDouble / measureIters
    println(f"平均実行時間: $average%,.0f ナノ秒")
  
  }

  // クヌースのアルゴリズムD (16ビット版)
  def divideKnuth16(uIn: BigInt, vIn: BigInt): (BigInt, BigInt) = {
    val BIT_SIZE = 16
    val BASE = 1L << BIT_SIZE
    val MASK = BASE - 1

    def toDigits(n: BigInt): Array[Long] = {
      val res = collection.mutable.ArrayBuffer[Long]()
      var temp = n
      while (temp > 0) { res.prepend((temp & MASK).toLong); temp >>= BIT_SIZE }
      if (res.isEmpty) Array(0L) else res.toArray
    }

    val uOrig = toDigits(uIn)
    val vOrig = toDigits(vIn)
    val n = vOrig.length
    val m = uOrig.length - n + 1

    var d = 0
    var tempV = vOrig(0)
    while (tempV < (1L << (BIT_SIZE - 1))) { tempV <<= 1; d += 1 }
    
    val vNorm = toDigits(vIn << d).takeRight(n)
    var uNorm = toDigits(uIn << d)
    if (uNorm.length <= n + m - 1) uNorm = Array.fill(n + m - uNorm.length)(0L) ++ uNorm
    uNorm = 0L +: uNorm

    val q = new Array[Long](m)

    // for (j = 0 to m)
    for (j <- 0 until m) {
      var qHat = ((uNorm(j) << BIT_SIZE) + uNorm(j + 1)) / vNorm(0)
      var rHat = ((uNorm(j) << BIT_SIZE) + uNorm(j + 1)) % vNorm(0)

      var isDone = false
      while (!isDone) {
        if (qHat >= BASE || (n > 1 && qHat * vNorm(1) > (rHat << BIT_SIZE) + uNorm(j + 2))) {
          qHat -= 1; rHat += vNorm(0)
          if (rHat >= BASE) isDone = true
        } else { isDone = true }
      }

      var borrow = 0L
      for (i <- n - 1 to 0 by -1) {
        val p = qHat * vNorm(i) + borrow
        var sub = uNorm(j + i + 1) - (p & MASK)
        if (sub < 0) { sub += BASE; borrow = (p >>> BIT_SIZE) + 1 }
        else { borrow = p >>> BIT_SIZE }
        uNorm(j + i + 1) = sub
      }

      if (uNorm(j) < borrow) {
        qHat -= 1
        var carry = 0L
        for (i <- n - 1 to 0 by -1) {
          val sum = uNorm(j + i + 1) + vNorm(i) + carry
          uNorm(j + i + 1) = sum & MASK
          carry = sum >>> BIT_SIZE
        }
      }
      q(j) = qHat
    }
    val qBig = q.foldLeft(BigInt(0))((acc, digit) => (acc << BIT_SIZE) + digit)
    val rBig = uNorm.takeRight(n).foldLeft(BigInt(0))((acc, digit) => (acc << BIT_SIZE) + digit) >> d
    (qBig, rBig)
  }
}