package tsframe

import tsframe.DTW._
import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

import org.scalacheck.Gen
import org.scalacheck.Prop
import org.scalacheck.Prop.{ forAll, exists }

class DTWSuite extends FunSuite with Checkers {
    def dist(a: MDVector, b: MDVector): Double = (a - b).magnitudeSquared
    val INF: Double = scala.Double.MaxValue

    class TSNorm(val ts: Array[MDVector]) {
        val size: Int = ts.size
        val mean: MDVector = ts.reduce(_ + _) / size
        val variance: MDVector = ts.map(x => x * x).reduce(_ + _) / size - mean * mean
        val std: MDVector = variance.sqrt()
        val ts_norm: Array[MDVector] = if(size == 1) ts else ts.map(x => (x - mean) / std)
    }

    def MDVectorGen(dimension: Int) = for {
        values <- Gen.containerOfN[Array, Double](dimension, Gen.choose(-100.0, 100.0))
    } yield new MDVector(values)

    test("A simple test") {
        // test case
        // note that A and B must have the same length because we use the cummulative bound obtained from LBKeogh
        def dist(a: Int, b: Int): Double = ((a - b) * (a - b)).toDouble
        val A = Array(1, 1, 2, 3, 2, 0)
        val B = Array(0, 1, 1, 2, 3, 2)
        val cb = Array.ofDim[Double](A.length)
        val window_size = A.length
        val d1 = DTWCalculator(dist)(A, B, cb, window_size, INF)
        val d2 = SimpleDTW(dist)(A, B)
        assert(d1 == d2)
    }

    test("SimpleDTW and DTWCalculator should return the same result for 1D data") {
        val arrayTupleGen = for {
            size <- Gen.choose(1, 200)
            A <- Gen.containerOfN[Array, Double](size, Gen.choose(-100.0, 100.0))
            B <- Gen.containerOfN[Array, Double](size, Gen.choose(-100.0, 100.0))
        } yield (A, B)
        def dist(a: Double, b: Double): Double = (a - b) * (a - b)
        val property = forAll(arrayTupleGen) { tuple =>
            val A = tuple._1
            val B = tuple._2
            val cb = Array.ofDim[Double](A.length)
            val r = A.length
            val d1 = DTWCalculator(dist)(A, B, cb, r, INF)
            val d2 = SimpleDTW(dist)(A, B)
            d1 == d2
        }
        check(property)
    }

    test("SimpleDTW and DTWCalculator shoudl return the same result for multi-D data") {

        val arrayTupleGen = for {
            size <- Gen.choose(1, 200)
            dimension <- Gen.choose(1, 20)
            A <- Gen.containerOfN[Array, MDVector](size, MDVectorGen(dimension))
            B <- Gen.containerOfN[Array, MDVector](size, MDVectorGen(dimension))
        } yield (A, B)


        val property = forAll(arrayTupleGen) { tuple =>
            val A = tuple._1
            val B = tuple._2
            val d1 = DTWCalculator(dist)(A, B, Array.ofDim[Double](A.length), A.length, INF)
            val d2 = SimpleDTW(dist)(A, B)
            d1 == d2
        }
        check(property)
    }


    test("MDEnvelope") {
        import scala.math.{ min, max, abs }
        def simpleEnvelope(ts: Array[Double], window_size: Int): (Array[Double], Array[Double]) = {
            if (ts.size == 0) (Array[Double](), Array[Double]())
            else {
                val r: Int = min(window_size, ts.size - 1)
                val windows = for (i <- 0 until ts.size) yield ts.slice(max(0, i - r), min(ts.size, i + r + 1))
                val upper = windows map { w => w reduce { (a, b) => if (a < b) b else a } }
                val lower = windows map { w => w reduce { (a, b) => if (a < b) a else b } }
                (upper.toArray, lower.toArray)
            }
        }

        val MDVectorArrayGen = for {
            size <- Gen.choose(1, 1000)
            dimension <- Gen.choose(1, 20)
            A <- Gen.containerOfN[Array, MDVector](size, MDVectorGen(dimension))
        } yield A

        val property = forAll(MDVectorArrayGen) { A: Array[MDVector] =>
            val r: Int = abs(scala.util.Random.nextInt) % A.size
            val (upper, lower) = MDEnvelope(A, r)
            // naive way to calculate the envelope
            (0 until A(0).dimension) forall { d => 
                val ts = A.map(_(d))
                val (u, l) = simpleEnvelope(ts, r)
                (0 until A.length) forall { i => upper(i)(d) == u(i) && lower(i)(d) == l(i) }
            }
        }

        check(property, minSuccessful(10))
    }


    test("The LBKim lower bound should be less than the DTW distance") {
        val ParametersGen = for {
            dimension <- Gen.choose(1, 20)
            size <- Gen.choose(6, 1000) // since we use at most 3 points at front and back, the size should not be less than 6
            candidate <- Gen.containerOfN[Array, MDVector](size, MDVectorGen(dimension))
            query <- Gen.containerOfN[Array, MDVector](size, MDVectorGen(dimension))
            r <- Gen.choose(2, size) // since we use at most 3 points at front and back, r should not be less than 2
        } yield (candidate, query, r)

        val property = forAll(ParametersGen) { params: (Array[MDVector], Array[MDVector], Int) =>
            val C = new TSNorm(params._1)
            val (c_norm, mean, std) = (C.ts_norm, C.mean, C.std)
            val q_norm = (new TSNorm(params._2)).ts_norm
            val r = params._3
            val distance = DTWCalculator(dist)(c_norm, q_norm, Array.ofDim[Double](c_norm.length), r, INF)
            val lb_kim = LBKim(dist)(C.ts, 0, q_norm, mean, std, INF)
            lb_kim <= distance
        }
        check(property)
    }

    test("The LBKoegh lower bound should be less than the DTW distance") {
        val ParametersGen = for {
            dimension <- Gen.choose(1, 20)
            size <- Gen.choose(1, 1000)
            candidate <- Gen.containerOfN[Array, MDVector](size, MDVectorGen(dimension))
            query <- Gen.containerOfN[Array, MDVector](size, MDVectorGen(dimension))
            r <- Gen.choose(0, size)
        } yield (candidate, query, r)

        val property = forAll(ParametersGen) { params: (Array[MDVector], Array[MDVector], Int) =>
            val C = new TSNorm(params._1)
            val (c_norm, mean, std) = (C.ts_norm, C.mean, C.std)
            val Q = new TSNorm(params._2)
            val q_norm = Q.ts_norm
            val r = params._3
            val (upper, lower) = MDEnvelope(q_norm, r)
            val distance = DTWCalculator(dist)(c_norm, q_norm, Array.ofDim[Double](c_norm.length), r, INF)
            val lb_koegh = LBKoegh(C.ts, 0, upper, lower, mean, std, INF)._2
            lb_koegh <= distance
        }

        check(property, minSuccessful(100))
    }
}
