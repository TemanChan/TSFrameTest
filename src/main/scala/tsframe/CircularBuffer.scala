package tsframe

class CircularBuffer[T : reflect.ClassTag](val _capacity: Int) extends java.io.Serializable {
    val _values: Array[T] = Array.ofDim[T](_capacity)

    var _first: Int = 0
    var size: Int = 0

    def apply(index: Int): T = {
        require(index >= 0 && index < size, "Index out of boundary error")
        val i = (_first + index) % _capacity
        _values(i)
    }

    def update(index: Int, value: T): Unit = {
        require(index >= 0 && index < size, "Index out of boundary error")
        val i = (_first + index) % _capacity
        _values(i) = value
    }

    def isEmpty(): Boolean = size == 0
    def pushBack(value: T): Unit = {
        require(size < _capacity, "Buffer already full")
        val index = (_first + size) % _capacity
        _values(index) = value
        size += 1
    }
    def popBack(): Unit = {
        require(size > 0, "Index out of boundary")
        size -= 1
    }
    def popFront(): Unit = {
        require(size > 0, "Index out of boundary")
        _first += 1
        if(_first == _capacity) _first = 0
        size -= 1
    }
    def front(): T = {
        require(size > 0, "Index out of boundary")
        _values(_first)
    }
    def back(): T = {
        require(size > 0, "Index out of boundary")
        val index = (_first + size - 1) % _capacity
        _values(index)
    }

    def clear(): Unit = {
        size = 0
    }
}
