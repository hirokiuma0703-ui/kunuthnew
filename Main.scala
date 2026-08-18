object Main {
  def main(args: Array[String]): Unit = {
    // --- 1. 定数とテストデータの準備 ---
    // 2048ビット級の巨大な数値（被除数 u と 除数 v）
    val u = BigInt("99999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999")
    val v = BigInt("1000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000009")

    val warmUpIters = 500  // JVM最適化のための空回し回数
    val measureIters = 1000 // 実際の計測回数

    // --- 2. ベンチマークの実行 ---
    println("--- クヌースのアルゴリズム (16bit) 計測開始 ---")
    
    // ウォームアップ：JITコンパイラによる最適化を促す
    for (_ <- 1 to warmUpIters) divideKnuth16(u, v)

    // 本計測：実行時間をナノ秒単位で計測
    val start = System.nanoTime()
    for (_ <- 1 to measureIters) divideKnuth16(u, v)
    val end = System.nanoTime()

    // 結果表示
    val average = (end - start).toDouble / measureIters
    println(f"平均実行時間: $average%,.0f ナノ秒")
  }

  /**
   * クヌースのアルゴリズムD (多倍長除算) の16ビット基数実装
   */
  def divideKnuth16(uIn: BigInt, vIn: BigInt): (BigInt, BigInt) = {
    // --- 3. 内部定数の定義 ---
    val BIT_SIZE = 16          // 1桁を16ビットとする
    val BASE = 1L << BIT_SIZE  // 基数（65536）
    val MASK = BASE - 1        // 桁を取り出すためのマスク

    // --- 4. 補助関数：BigIntを16ビットごとの配列に変換 ---
    def toDigits(n: BigInt): Array[Long] = {
      val res = collection.mutable.ArrayBuffer[Long]()
      var temp = n
      while (temp > 0) { 
        res.prepend((temp & MASK).toLong) // 下位16ビットを抽出して先頭に追加
        temp >>= BIT_SIZE 
      }
      if (res.isEmpty) Array(0L) else res.toArray
    }

    // --- 5. 前処理と正規化 (Normalization) ---
    // アルゴリズムの精度を高めるため、除数vの最上位桁がBASE/2以上になるようシフトする
    val uOrig = toDigits(uIn)
    val vOrig = toDigits(vIn)
    val n = vOrig.length // 除数の桁数
    val m = uOrig.length - n + 1 // 商の桁数

    var d = 0
    var tempV = vOrig(0)
    // 最上位ビットが立つまでシフト量を計算
    while (tempV < (1L << (BIT_SIZE - 1))) { tempV <<= 1; d += 1 }
    
    // 正規化された数値を作成
    val vNorm = toDigits(vIn << d).takeRight(n)
    var uNorm = toDigits(uIn << d)
    // 桁合わせ（uの長さを調整）
    if (uNorm.length <= n + m - 1) uNorm = Array.fill(n + m - uNorm.length)(0L) ++ uNorm
    uNorm = 0L +: uNorm // 計算用に先頭に0を追加

    val q = new Array[Long](m) // 商を格納する配列

    // --- 6. メインループ：桁ごとの除算計算 ---
    for (j <- 0 until m) {
      // (1) 商の予測値 qHat を計算
      var qHat = ((uNorm(j) << BIT_SIZE) + uNorm(j + 1)) / vNorm(0)
      var rHat = ((uNorm(j) << BIT_SIZE) + uNorm(j + 1)) % vNorm(0)

      // (2) 予測値 qHat の修正
      // qHatが大きすぎる場合、1減らして調整を繰り返す
      var isDone = false
      while (!isDone) {
        if (qHat >= BASE || (n > 1 && qHat * vNorm(1) > (rHat << BIT_SIZE) + uNorm(j + 2))) {
          qHat -= 1; rHat += vNorm(0)
          if (rHat >= BASE) isDone = true // rHatがBASEを超えたらこれ以上引けない
        } else { isDone = true }
      }

      // (3) 乗算と減算： u = u - (qHat * v)
      var borrow = 0L
      for (i <- n - 1 to 0 by -1) {
        val p = qHat * vNorm(i) + borrow
        var sub = uNorm(j + i + 1) - (p & MASK)
        if (sub < 0) { 
          sub += BASE
          borrow = (p >>> BIT_SIZE) + 1 
        } else { 
          borrow = p >>> BIT_SIZE 
        }
        uNorm(j + i + 1) = sub
      }

      // (4) 引きすぎた場合の補正（Add back）
      // 結果が負になった（borrowが残った）場合、qHatを1減らし、vを足し戻す
      if (uNorm(j) < borrow) {
        qHat -= 1
        var carry = 0L
        for (i <- n - 1 to 0 by -1) {
          val sum = uNorm(j + i + 1) + vNorm(i) + carry
          uNorm(j + i + 1) = sum & MASK
          carry = sum >>> BIT_SIZE
        }
      }
      q(j) = qHat // 確定した商の桁を保存
    }

    // --- 7. 結果の復元 ---
    // 16ビットの配列を一つのBigIntに戻す
    val qBig = q.foldLeft(BigInt(0))((acc, digit) => (acc << BIT_SIZE) + digit)
    // 余りは、正規化時にシフトした分(d)を右シフトして戻す
    val rBig = uNorm.takeRight(n).foldLeft(BigInt(0))((acc, digit) => (acc << BIT_SIZE) + digit) >> d
    
    (qBig, rBig) // (商, 余り) のペアを返す
  }
}
