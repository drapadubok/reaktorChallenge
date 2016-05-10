import scala.io.Source
import scala.collection.immutable.Map


case class Point3D(x: Double, y: Double, z: Double)
case class PointSphere(lat: Double, lng: Double, alt: Double)


def Data(url: String): String = {
  /**
  Grab data from url.
  */
  Source.fromURL(url).mkString
}


def parseData(data: String): (String, Array[Array[String]]) = {
  /**
  Convert data into (Seed, Array of records)
  */
  val splitData = data.split("\n")

  val seed = splitData(0).split(" ")(1)
  val route = splitData.last.split(",")

  val routeStart = Array("Start", route(1), route(2), "10")
  val routeEnd = Array("End", route(3), route(4), "10")

  val tempdata = splitData.drop(1).dropRight(1).map(_.split(","))

  var fullData = routeStart +: tempdata :+ routeEnd
  (seed, fullData)
}


def toRadians(a: Double): Double = a * Math.PI / 180.0


def convertToCartesian(point: PointSphere, radius: Double): Point3D = {
  /**
   * order in input data: latitude,longitude,altitude
  http://gis.stackexchange.com/questions/4147/lat-lon-alt-to-spherical-or-cartesian-coordinates
  I add altitude to radius to shift orbit so that the conventional formula still makes sense.
  Just assume the point is on the sphere with larger radius.
  */

  val adjustedRadius = radius + point.alt
  val lat = toRadians(point.lat)
  val lng = toRadians(point.lng)
  
  val x = adjustedRadius * Math.cos(lat) * Math.cos(lng)
  val y = adjustedRadius * Math.cos(lat) * Math.sin(lng)
  val z = adjustedRadius * Math.sin(lat)
  
  Point3D(x, y, z)
}


def isIntersectingGlobe(a: Point3D, b: Point3D, c: Point3D, radius: Double): Boolean = {
  // http://stackoverflow.com/questions/5883169/intersection-between-a-line-and-a-sphere
  // c - sphere center, assuming origin (0,0,0)
  val cx = c.x
  val cy = c.y
  val cz = c.z

  val px = a.x
  val py = a.y
  val pz = a.z

  val vx = b.x - px
  val vy = b.y - py
  val vz = b.z - pz

  val A = vx * vx + vy * vy + vz * vz
  val B = 2.0 * (px * vx + py * vy + pz * vz - vx * cx - vy * cy - vz * cz)
  val C = px * px - 2 * px * cx + cx * cx + py * py - 2 * py * cy + cy * cy + pz * pz - 2 * pz * cz + cz * cz - radius * radius

  val discriminant = (B * B) - (4 * A * C)
  // if discriminant > 0: two intersection points
  // if discriminant == 0: one intersection point
  // if discriminant < 0: no intersection points

  discriminant >= 0
}


def vectorLength(a: Point3D, b: Point3D): Double = {
  val vx = b.x - a.x
  val vy = b.y - a.y
  val vz = b.z - a.z
  Math.sqrt(vx*vx + vy*vy + vz*vz)
}


def getAdjacencyMatrix(finalData: Map[Int, Point3D], c: Point3D, radius: Double): Array[Array[Double]] = {
  val ndim = finalData.size
  val AdjacencyMatrix = Array.fill(ndim, ndim)(0.0)
  // for every node, check if it is possible to reach every other node
  val allnodes = (0 until ndim)
  for (i <- allnodes; j <- allnodes) {
    if (i != j) {
      if ( isIntersectingGlobe( finalData(i), finalData(j), c, radius ) ) { 
        AdjacencyMatrix(i)(j) = 0.0
      } else {
        val vlen = vectorLength(finalData(i), finalData(j))
        AdjacencyMatrix(i)(j) = vlen
        AdjacencyMatrix(j)(i) = vlen
        println("Found link between nodes: " + i + " and " + j)
      }
    }
  }
  AdjacencyMatrix
}


def minDistance(dist: Array[Double], Q: Array[Int]): Int = {
  var min = Double.MaxValue
  var min_index = -1

  for (v <- (0 until dist.length) ) {
    if (Q.contains(v) && dist(v) <= min) {
      min = dist(v)
      min_index = v
    }
  }
  min_index
}


def Dijkstra(AdjacencyMatrix: Array[Array[Double]], source: Int, ndim: Int): (Array[Double], Array[Int]) = {
  var Q = (0 until ndim).toArray
  val dist = Array.fill(ndim)(Double.MaxValue)
  // tree
  val previous = Array.fill(ndim)(999999)

  dist(source) = 0.0

  while (Q.size > 0) {
    var u = minDistance(dist, Q)
    Q = Q.filterNot(_ == u)

    for ( v <- (0 until ndim) ) {
      // if neighbour
      if ( Q.contains(v) && AdjacencyMatrix(u)(v) != 0.0 ) {
        println("hit")
        val alt = dist(u) + AdjacencyMatrix(u)(v)
        if (alt < dist(v)) {
          dist(v) = alt
          previous(v) = u
        }
      }
    }
  }
  (dist, previous)
}


def constructPath(shortestPath: (Array[Double], Array[Int]), target: Int, undefined: Int): (Double, Array[Int]) = {
  /**
  shortestPath - output of Dijkstra
  target - target node
  undefined - Integer specifying what value was used to mark 'undefined', used Wiki for algorithm

  Trace back the path, if node is reachable.
  */
  var S = Array[Int]()
  var u = target

  val previous = shortestPath._2

  while (previous(u) != 999999) {
    S = S :+ u
    u = previous(u)
  }
  S = S :+ u

  (shortestPath._1(target), S)
}


// Definitions done, now with actual processing
val url = "https://space-fast-track.herokuapp.com/generate"
val data = Data(url)
val parsedData = parseData(data)

val radius = 6371.0
val c = Point3D(0.0, 0.0, 0.0)

val seed = parsedData._1
val fullData = parsedData._2

val dataPoints = fullData.map { line =>
  (line(0), PointSphere(line(1).toDouble, line(2).toDouble, line(3).toDouble) )
}

// convert satellite names to Integer Ids, convenient later
val uniqueNames = dataPoints.map( i => i._1 ).zipWithIndex.toMap
val finalData = dataPoints.map { i =>
  ( uniqueNames(i._1), convertToCartesian(i._2, radius) )
}.toMap

val ndim = finalData.size
val AdjacencyMatrix = getAdjacencyMatrix(finalData, c, radius)


// important test for data usability
val a = finalData(0)
val b = finalData(21)
isIntersectingGlobe(a, b, c, radius)
println(AdjacencyMatrix(0).sum != 0 && AdjacencyMatrix(21).sum != 0)

val shortestPath = Dijkstra(AdjacencyMatrix, 0, ndim)
val result = constructPath(shortestPath, 21, 999999)
