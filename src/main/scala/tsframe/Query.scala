package tsframe

class OrderedQuery(val query: Array[MDVector]) extends java.io.Serializable {
    val mean: MDVector = query.reduce(_ + _) / query.length
    val variance: MDVector = query.map(x => x * x).reduce(_ + _) / query.length - mean * mean
    val std: MDVector = variance.sqrt()
    val query_norm: Array[MDVector] = query.map(x => (x - mean) / std)
    val (a, b) = query_norm.zipWithIndex.sortWith{ (left, right) => left._1 > right._1 }.unzip
    val (query_ordered: Array[MDVector], order: Array[Int]) = (a.toArray, b.toArray)
}