package tsframe

import tsframe.DTW._
import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

import org.scalacheck.Gen
import org.scalacheck.Prop
import org.scalacheck.Prop.forAll

class DTWSuite extends FunSuite with Checkers {

    def MDVectorGen(dimension: Int) = for {
        values <- Gen.containerOfN[Array, Double](dimension, Gen.choose(-100.0, 100.0))
    } yield new MDVector(values)

    test("A simple test"){
        // test case
        // note that A and B must have the same length because we use the cummulative bound obtained from LBKeogh
        val A = Array(1, 1, 2, 3, 2, 0)
        val B = Array(0, 1, 1, 2, 3, 2)
        val cb = Array.ofDim[Double](A.length)
        val window_size = A.length
        val bsf = scala.Double.MaxValue
        val dist = (a: Int, b: Int) => ((a - b) * (a - b)).toDouble
        val d1 = DTWCalculator(dist)(A, B, cb, window_size, bsf) 
        val d2 = SimpleDTW(dist)(A, B)
        assert(d1 == d2)
    }

    test("SimpleDTW and DTWCalculator should return the same result for 1D data") {
        val bsf = scala.Double.MaxValue
        val dist = (a: Double, b: Double) => (a - b) * (a - b)
        val arrayTupleGen = for {
            size <- Gen.choose(1, 200)
            A <- Gen.containerOfN[Array, Double](size, Gen.choose(-100.0, 100.0))
            B <- Gen.containerOfN[Array, Double](size, Gen.choose(-100.0, 100.0))
        } yield (A, B)
        val property = forAll(arrayTupleGen){ tuple =>
            val A = tuple._1
            val B = tuple._2
            val cb = Array.ofDim[Double](A.length)
            val r = A.length
            val d1 = DTWCalculator(dist)(A, B, cb, r, bsf)
            val d2 = SimpleDTW(dist)(A, B)
            d1 == d2
        }
        check(property)
    }

    test("SimpleDTW and DTWCalculator shoudl return the same result for multi-D data"){

        val arrayTupleGen = for {
            size <- Gen.choose(1, 200)
            dimension <- Gen.choose(1, 20)
            A <- Gen.containerOfN[Array, MDVector](size, MDVectorGen(dimension))
            B <- Gen.containerOfN[Array, MDVector](size, MDVectorGen(dimension))
        } yield (A, B)
        
        def dist(a: MDVector, b: MDVector): Double = (a - b).magnitudeSquared

        val property = forAll(arrayTupleGen){ tuple =>
            val A = tuple._1
            val B = tuple._2
            val d1 = DTWCalculator(dist)(A, B, Array.ofDim[Double](A.length), A.length, scala.Double.MaxValue)
            val d2 = SimpleDTW(dist)(A, B)
            d1 == d2
        }
        check(property)
    }

    test("envelope"){
        val MDVectorArrayGen = for {
            dimension <- Gen.choose(1, 20)
            A <- Gen.containerOf[Array, MDVector](MDVectorGen(dimension))
        } yield A

        val property = forAll(MDVectorArrayGen){ A: Array[MDVector] =>
            val (upper, lower) = envelope(A, A.length / 10)
            upper.length == A.length && lower.length == A.length
        }

        check(property)
    }
}
