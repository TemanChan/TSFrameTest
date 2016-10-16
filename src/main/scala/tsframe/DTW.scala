package tsframe

import scala.math.{min, max}

object DTW extends java.io.Serializable {
    def SimpleDTW[T](dist: (T, T) => Double)(A: Array[T], B: Array[T]): Double = {
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
        val INF: Double = scala.Double.MaxValue
        val m: Int = A.length
        val r: Int = min(window_size, m-1)
        var cost: Array[Double] = Array.fill[Double](2 * r + 1)(INF)
        cost(r) = 0
        for(i <- 0 until m){
            var k: Int = max(0, r-i)
            var min_cost: Double = INF
            for(j <- max(0, i-r) to min(m-1, i+r)){
                // costs(i)(j-1)
                val x: Double = if(k > 0) cost(k-1) else INF
                // costs(i-1)(j-1)
                val y: Double = cost(k)
                // costs(i-1)(j)
                val z: Double = if(k < 2*r) cost(k+1) else INF
                cost(k) = min(x, min(y, z)) + dist(A(i), B(j))
                if(cost(k) < min_cost) min_cost = cost(k)
                k += 1
            }
            if(i+r < m-1 && min_cost + cb(i+r+1) >= bsf){
                return min_cost + cb(i+r+1)
            }
        }
        cost(r)
    }

    def envelope[T <% Ordered[T] : reflect.ClassTag](time_series: Array[T], window_size: Int): (Array[T], Array[T]) = {
        val size: Int = time_series.length
        val upper: Array[T] = Array.ofDim[T](size)
        val lower: Array[T] = Array.ofDim[T](size)
        val du = new CircularBuffer[Int](2 * window_size + 2)
        val dl = new CircularBuffer[Int](2 * window_size + 2)
        du.pushBack(0)
        dl.pushBack(0)

        for (i <- 1 until size) {
            if (i > window_size) {
                upper(i - window_size - 1) = time_series(du.front())
                lower(i - window_size - 1) = time_series(dl.front())
            }

            if (time_series(i - 1) < time_series(i)) {
                du.popBack()
                while (!du.isEmpty() && time_series(du.back()) < time_series(i))
                    du.popBack()
            } else {
                dl.popBack()
                while (!dl.isEmpty() && time_series(i) < time_series(dl.back()))
                    dl.popBack()
            }

            du.pushBack(i)
            dl.pushBack(i)

            if (i == 2 * window_size + 1 + du.front())
                du.popFront()
            else if (i == 2 * window_size + 1 + dl.front())
                dl.popFront()
        }

        for (i <- size to (size + window_size)) {
            upper(i - window_size - 1) = time_series(du.front())
            lower(i - window_size - 1) = time_series(dl.front())
            if (i - du.front() >= 2 * window_size + 1)
                du.popFront();
            if (i - dl.front() >= 2 * window_size + 1)
                dl.popFront();
        }

        (upper, lower)
    }

    def LBKim(dist: (MDVector, MDVector) => Double)(candidate: Array[MDVector], start_index: Int, query: Array[MDVector], mean: MDVector, std: MDVector, bsf: Double): Double = {
        def min(a: Double, b: Double) = scala.math.min(a, b)
        def normalize(x: MDVector) = (x - mean) / std

        // since we double the size of circular buffer to avoid using %, we need to half the size here
        val clen: Int = candidate.length / 2
        val qlen: Int = query.length
        // 1 point at front and back
        val x0: MDVector = normalize(candidate(start_index))
        val y0: MDVector = normalize(candidate(start_index + clen - 1))
        var lb: Double = dist(x0, query(0)) + dist(y0, query(qlen - 1))

        if (lb < bsf) {
            // 2 points at front
            val x1: MDVector = normalize(candidate(start_index + 1))
            val d: Double = Array[Double](dist(x1, query(0)), dist(x0, query(1)), dist(x1, query(1))).reduce(min)
            lb += d
            if (lb < bsf) {
                // 2 points at back
                val y1: MDVector = normalize(candidate(start_index + clen - 2))
                val d: Double = Array[Double](dist(y1, query(qlen - 1)), dist(y0, query(qlen - 2)), dist(y1, query(qlen - 2))).reduce(min);
                lb += d
                if (lb < bsf) {
                    // 3 points at front
                    val x2: MDVector = normalize(candidate(start_index + 2))
                    val d: Double = Array[Double](dist(x0, query(2)), dist(x1, query(2)), dist(x2, query(2)),
                        dist(x2, query(1)), dist(x2, query(0))).reduce(min)
                    lb += d
                    if (lb < bsf) {
                        // 3 points at back
                        val y2: MDVector = normalize(candidate(start_index + clen - 3))
                        val d: Double = Array[Double](dist(y0, query(qlen - 3)), dist(y1, query(qlen - 3)), dist(y2, query(qlen - 3)),
                            dist(y2, query(qlen - 2)), dist(y2, query(qlen - 1))).reduce(min)
                        lb += d
                    }
                }
            }
        }
        lb
    }

    /**
     * @param candidate A circular array storing the candidate time series
     * @param start_index The start index of the circular array
     * @param order The order in which the query time series is sorted
     * @param upper_ordered The upper envelope of sorted query
     * @param lower_ordered The lower envelope of sorted query
     * @param mean The mean of current candidate
     * @param std The standart deviation of current candidate
     * @param bsf The best-so-far distance
     * @return cb Current bound at each position which will be used later for early abandoning
     * @return lb The Koegh lower bound of the DTW distance
     */
    def LBKoegh(dist: (MDVector, MDVector) => Double)(candidate: Array[MDVector], start_index: Int, order: Array[Int], upper_ordered: Array[MDVector], lower_ordered: Array[MDVector], mean: MDVector, std: MDVector, bsf: Double): (Array[Double], Double) = {
        val len: Int = order.length
        val cb: Array[Double] = Array.ofDim[Double](len)
        var lb: Double = 0
        var i: Int = 0
        while (i < len && lb < bsf) {
            val x: MDVector = (candidate(order(i) + start_index) - mean) / std
            val d: Double = x match {
                case _ if x > upper_ordered(i) => dist(x, upper_ordered(i))
                case _ if x < lower_ordered(i) => dist(x, lower_ordered(i))
                case _ => 0
            }
            lb += d
            cb(order(i)) = d
            i += 1
        }
        (cb, lb)
    }

    /**
     * @param upper The upper envelope of current candidate
     * @param lower The lower envelope of current candidate
     * @param query_order The sorted query
     * @param order The order in which query is sorted
     * @param mean The mean of current candidate
     * @param std The standard deviation of current candidate
     * @param bsf The best-so-far distance
     * @return cb Current bound at each position which will be used later for early abandoning
     * @return lb Lower bound of the DTW distance
     */
    def LBKoegh2(dist: (MDVector, MDVector) => Double)(upper: Array[MDVector], lower: Array[MDVector], query_ordered: Array[MDVector], order: Array[Int], mean: MDVector, std: MDVector, bsf: Double): (Array[Double], Double) = {
        val len: Int = order.length
        val cb: Array[Double] = Array.ofDim[Double](len)
        var lb: Double = 0
        var i: Int = 0
        while (i < len && lb < bsf) {
            val u_norm: MDVector = (upper(order(i)) - mean) / std
            val l_norm: MDVector = (lower(order(i)) - mean) / std
            val qi: MDVector = query_ordered(i)
            val d: Double = qi match {
                case _ if qi > u_norm => dist(qi, u_norm)
                case _ if qi < l_norm => dist(qi, l_norm)
                case _ => 0
            }
            lb += d
            cb(order(i)) = d
            i += 1
        }
        (cb, lb)
    }

}