package tsframe

import scala.math.{ min, max }

object DTW extends java.io.Serializable {
    def SimpleDTW[T](dist: (T, T) => Double)(A: Array[T], B: Array[T]): Double = {
        val costs: Array[Array[Double]] = Array.ofDim[Double](A.length, B.length)
        for (
            i <- 0 until A.length;
            j <- 0 until B.length
        ) {
            costs(i)(j) = dist(A(i), B(j))
        }
        for (i <- 1 until A.length) {
            costs(i)(0) += costs(i - 1)(0)
        }
        for (j <- 1 until B.length) {
            costs(0)(j) += costs(0)(j - 1)
        }
        for (
            i <- 1 until A.length;
            j <- 1 until B.length
        ) {
            costs(i)(j) += min(costs(i)(j - 1), min(costs(i - 1)(j - 1), costs(i - 1)(j)))
        }
        costs(A.length - 1)(B.length - 1)
    }

    def DTWCalculator[T](dist: (T, T) => Double)(A: Array[T], B: Array[T], cb: Array[Double], window_size: Int, bsf: Double): Double = {
        val INF: Double = scala.Double.MaxValue
        val m: Int = A.length
        val r: Int = min(window_size, m - 1)
        var cost: Array[Double] = Array.fill[Double](2 * r + 1)(INF)
        cost(r) = 0
        for (i <- 0 until m) {
            var k: Int = max(0, r - i)
            var min_cost: Double = INF
            for (j <- max(0, i - r) to min(m - 1, i + r)) {
                // costs(i)(j-1)
                val x: Double = if (k > 0) cost(k - 1) else INF
                // costs(i-1)(j-1)
                val y: Double = cost(k)
                // costs(i-1)(j)
                val z: Double = if (k < 2 * r) cost(k + 1) else INF
                cost(k) = min(x, min(y, z)) + dist(A(i), B(j))
                if (cost(k) < min_cost) min_cost = cost(k)
                k += 1
            }
            if (i + r < m - 1 && min_cost + cb(i + r + 1) >= bsf) {
                return min_cost + cb(i + r + 1)
            }
        }
        cost(r)
    }

    def envelope[T <% Ordered[T]: reflect.ClassTag](time_series: Array[T], window_size: Int): (Array[T], Array[T]) = {
        val size: Int = time_series.length
        val upper: Array[T] = Array.ofDim[T](size)
        val lower: Array[T] = Array.ofDim[T](size)
        // the window includes r elements before (if any) and r elements after (if any) the current element
        // so r + 1 <= total size of window <= 2*r + 1
        // since r + 1 <= total size of window <= size of time_series
        // r <= size - 1
        val r: Int = min(window_size, size - 1)
        val du = new CircularBuffer[Int](2 * r + 2)
        val dl = new CircularBuffer[Int](2 * r + 2)

        if (size > 0) {
            du.pushBack(0)
            dl.pushBack(0)
        }

        for (i <- 1 until size) {
            if (i > r) {
                upper(i - r - 1) = time_series(du.front())
                lower(i - r - 1) = time_series(dl.front())
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

            if (i == 2 * r + 1 + du.front())
                du.popFront()
            else if (i == 2 * r + 1 + dl.front())
                dl.popFront()
        }

        for (i <- size to (size + r)) {
            upper(i - r - 1) = time_series(du.front())
            lower(i - r - 1) = time_series(dl.front())
            if (i - du.front() >= 2 * r + 1)
                du.popFront();
            if (i - dl.front() >= 2 * r + 1)
                dl.popFront();
        }

        (upper, lower)
    }

    def MDEnvelope(time_series: Array[MDVector], window_size: Int): (Array[Array[Double]], Array[Array[Double]]) = {
        type BufferType = CircularBuffer[Tuple2[Int, Double]]
        val size: Int = time_series.length
        val r: Int = min(window_size, size - 1)
        val dim: Int = time_series(0).dimension
        val upper: Array[Array[Double]] = Array.ofDim[Double](size, dim)
        val lower: Array[Array[Double]] = Array.ofDim[Double](size, dim)
        val du: Array[BufferType] = Array.fill[BufferType](dim)(new BufferType(2 * r + 2))
        val dl: Array[BufferType] = Array.fill[BufferType](dim)(new BufferType(2 * r + 2))

        for (d <- 0 until dim) {
            du(d).pushBack((0, time_series(0)(d)))
            dl(d).pushBack((0, time_series(0)(d)))
        }

        for (i <- 1 until size) {
            if (i > r) {
                for (d <- 0 until dim) {
                    upper(i - r - 1)(d) = du(d).front()._2
                    lower(i - r - 1)(d) = dl(d).front()._2
                }
            }

            val x = time_series(i)
            for (d <- 0 until dim) {
                if (du(d).back()._2 < x(d)) {
                    du(d).popBack()
                    while (!du(d).isEmpty() && du(d).back()._2 < x(d))
                        du(d).popBack()
                } else {
                    dl(d).popBack()
                    while (!dl(d).isEmpty() && dl(d).back()._2 > x(d))
                        dl(d).popBack()
                }

                du(d).pushBack((i, x(d)))
                dl(d).pushBack((i, x(d)))

                if (i == 2 * r + 1 + du(d).front()._1)
                    du(d).popFront()
                else if (i == 2 * r + 1 + dl(d).front()._1)
                    dl(d).popFront()
            }
        }

        for (i <- size to (size + r)) {
            for (d <- 0 until dim) {
                upper(i - r - 1)(d) = du(d).front()._2
                lower(i - r - 1)(d) = dl(d).front()._2
                if (i - du(d).front()._1 >= 2 * r + 1)
                    du(d).popFront();
                if (i - dl(d).front()._1 >= 2 * r + 1)
                    dl(d).popFront();
            }
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
    def LBKoegh(candidate: Array[MDVector], start_index: Int, upper: Array[Array[Double]], lower: Array[Array[Double]], mean: MDVector, std: MDVector, bsf: Double): (Array[Double], Double) = {
        def square(a: Double): Double = a * a
        val len: Int = upper.length
        val dim: Int = candidate(0).dimension
        val cb: Array[Double] = Array.ofDim[Double](len)
        var lb: Double = 0
        var i: Int = 0
        while (i < len && lb < bsf) {
            val x: MDVector = (candidate(i + start_index) - mean) / std
            val d: Double = (0 until dim) map { d =>
                if (x(d) > upper(i)(d)) square(x(d) - upper(i)(d))
                else if (x(d) < lower(i)(d)) square(x(d) - lower(i)(d))
                else 0.0
            } reduce (_ + _)
            lb += d
            cb(i) = d
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
    def LBKoegh2(upper: Array[Array[Double]], lower: Array[Array[Double]], query: Array[MDVector], mean: MDVector, std: MDVector, bsf: Double): (Array[Double], Double) = {
        def square(a: Double): Double = a * a
        val len: Int = query.length
        val dim: Int = query(0).dimension
        val u_norm: Array[MDVector] = upper.map(x => (new MDVector(x) - mean) / std)
        val l_norm: Array[MDVector] = lower.map(x => (new MDVector(x) - mean) / std)
        val cb: Array[Double] = Array.ofDim[Double](len)
        var lb: Double = 0
        var i: Int = 0
        while (i < len && lb < bsf) {
            val d: Double = (0 until dim) map { d =>
                if (query(i)(d) > u_norm(i)(d)) square(query(i)(d) - u_norm(i)(d))
                else if (query(i)(d) < l_norm(i)(d)) square(query(i)(d) - l_norm(i)(d))
                else 0
            } reduce (_ + _)
            lb += d
            cb(i) = d
            i += 1
        }
        (cb, lb)
    }

}