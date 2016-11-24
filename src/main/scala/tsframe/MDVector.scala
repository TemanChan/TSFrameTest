package tsframe

class MDVector(val _values: Array[Double]) extends java.io.Serializable with Ordered[MDVector] { // Bug of Zeppelin or Spark? Error if the left curly bracket { is placed at the next line
    val dimension: Int = _values.length

    def this(values: Double*) = this(values.toArray)
    def this(size: Int) = this(Array.ofDim[Double](size))

    def apply(index: Int): Double = _values(index)
    def update(index: Int, new_value: Double): Unit = _values(index) = new_value

    // return a new vector
    def +(that: MDVector): MDVector = {
        require(_values.size == that._values.size, "vector dimensions must match")
        val a = new Array[Double](_values.size)
        var i = 0
        while(i < _values.size){
            a(i) = _values(i) + that._values(i)
            i += 1
        }
        new MDVector(a)
    }
    def -(that: MDVector): MDVector = {
        require(_values.size == that._values.size, "vector dimensions must match")
        val a = new Array[Double](_values.size)
        var i = 0
        while(i < _values.size){
            a(i) = _values(i) - that._values(i)
            i += 1
        }
        new MDVector(a)
    }
    def *(that: MDVector): MDVector = {
        require(_values.size == that._values.size, "vector dimensions must match")
        val a = new Array[Double](_values.size)
        var i = 0
        while(i < _values.size){
            a(i) = _values(i) * that._values(i)
            i += 1
        }
        new MDVector(a)
    }
    def /(that: MDVector): MDVector = {
        require(_values.size == that._values.size, "vector dimensions must match")
        val a = new Array[Double](_values.size)
        var i = 0
        while(i < _values.size){
            a(i) = _values(i) / that._values(i)
            i += 1
        }
        new MDVector(a)
    }

    def /(d: Double): MDVector = {
        new MDVector(_values.map(_ / d))
    }

    // update this vector
    def +=(that: MDVector): MDVector = {
        require(_values.size == that._values.size, "vector dimensions must match")
        var i = 0
        while(i < _values.size){
            _values(i) = _values(i) + that._values(i)
            i += 1
        }
        this
    }
    def -=(that: MDVector): MDVector = {
        require(_values.size == that._values.size, "vector dimensions must match")
        var i = 0
        while(i < _values.size){
            _values(i) = _values(i) - that._values(i)
            i += 1
        }
        this
    }

    /*
    // return true iff the magnitude of this vector is less than the magnitude of that vector
    def <(that: MDVector): Boolean = {
        require(_values.size == that._values.size, "vector dimensions must match")
        _values.fold(0.0){ case (accu, x) => accu + x * x } < that._values.fold(0.0){ case (accu, x) => accu + x * x }
    }

    def >(that: MDVector): Boolean = that < this
    */
    def compare(that: MDVector): Int = {
        require(_values.size == that._values.size, "vector dimensions must match")
        _values.fold(0.0){ case (accu, x) => accu + x * x } compare that._values.fold(0.0){ case (accu, x) => accu + x * x }
    }

    // return a new vector
    def sqrt(): MDVector = {
        new MDVector(_values map scala.math.sqrt)
    }

    def absSum(): Double = {
        _values.map(scala.math.abs).reduce(_ + _)
    }

    def magnitudeSquared: Double = _values.map(x => x * x).reduce(_ + _)
    
    override def toString = "[" + _values.mkString(",") + "]"
}
