package tsframe

object DTW{
    def SimpleDTW[T](dist: (T, T) => Double)(A: Array[T], B: Array[T]): Double = {
        import scala.math.min
        val costs: Array[Array[Double]] = Array.ofDim[Double](A.length, B.length)
        for(i <- 0 until A.length;
            j <- 0 until B.length){
            costs(i)(j) = dist(A(i), B(j))
        }
        for(i <- 1 until A.length){
            costs(i)(0) += costs(i-1)(0)
        }
        for(j <- 1 until B.length){
            costs(0)(j) += costs(0)(j-1)
        }
        for(i <- 1 until A.length;
            j <- 1 until B.length){
            costs(i)(j) += min(costs(i)(j-1), min(costs(i-1)(j-1), costs(i-1)(j)))
        }
        costs(A.length-1)(B.length-1)
    }
    
    def DTWCalculator[T](dist: (T, T) => Double)(A: Array[T], B: Array[T], cb: Array[Double], window_size: Int, bsf: Double): Double = {
        import scala.math.{min, max}
        val INF: Double = scala.Double.MaxValue
        val m: Int = A.length
        val r: Int = min(window_size, m-1)
        var cost: Array[Double] = Array.fill[Double](2 * r + 1)(INF)
        var cost_prev: Array[Double] = Array.fill[Double](2 * r + 1)(INF)
        cost_prev(r) = 0
        for(i <- 0 until m){
            var k: Int = max(0, r-i)
            var min_cost: Double = INF
            for(j <- max(0, i-r) to min(m-1, i+r)){
                // costs(i)(j-1)
                val x: Double = if(k > 0) cost(k-1) else INF
                // costs(i-1)(j-1)
                val y: Double = cost_prev(k)
                // costs(i-1)(j)
                val z: Double = if(k < 2*r) cost_prev(k+1) else INF
                cost(k) = min(x, min(y, z)) + dist(A(i), B(j))
                if(cost(k) < min_cost) min_cost = cost(k)
                k += 1
            }
            if(i+r < m-1 && min_cost + cb(i+r+1) >= bsf){
                return min_cost + cb(i+r+1)
            }
            cost_prev = cost
        }
        cost(r)
    }
}