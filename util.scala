case class Point3D(x: Double, y: Double, z: Double)

def vectorLength(a: Point3D, b: Point3D): Double = {
  val vx = b.x - a.x
  val vy = b.y - a.y
  val vz = b.z - a.z
  Math.sqrt(vx*vx + vy*vy + vz*vz)
}

def isIntersectingGlobe(a: Point3D, b: Point3D, c: Point3D, radius: Double): Boolean = {
  var retval = false
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
  val B = 2.0 * (px * vx + py * vy + pz * vz + vx * cx - vy * cy - vz * cz)
  val C = px * px - 2 * px * cx + cx * cx + py * py - 2 * py * cy + cy * cy + pz * pz - 2 * pz * cz + cz * cz - radius * radius

  val discriminant = B * B - 4 * A * C
  
  // if discriminant > 0: two intersection points
  // if discriminant == 0: one intersection point
  // if discriminant < 0: no intersection points
  if ( discriminant >= 0 ) { retval = true }

  retval

  /*
  // Solutions, not needed here
  val t1 = ( -B - Math.sqrt(discriminant) ) / (2.0 * A)
  val tt1 = 1 - t1
  // if D == 0
  val solution1 = ( a.x * tt1 + t1 * b.x, a.y * tt1 + t1 * b.y, a.z * tt1 + t1 * b.z )
  // if D > 0
  val t2 = ( -B + Math.sqrt(discriminant) ) / (2.0 * A)
  val tt2 = 1 - t2
  val solution2 = (a.x * tt2 + t2 * b.x, a.y * tt2 + t2 * b.y, a.z * tt2 + t2 * b.z ) 
  */
}

/*
Small utility to add radius to every coordinate, to make sure the points are OUTSIDE the globe.
*/
def addRadius(p: Point3D, radius: Double): Point3D = {
  def util(i: Double): Double = {
    if (i >= 0.0) {
      i + radius
    } else {
      i - radius
    }
  }
  Point3D(util(p.x), util(p.y), util(p.z))
}


val input = scala.io.Source.fromFile("/m/nbe/home/smirnod1/reaktor.csv").mkString.split("\n")

val radius = 6371.0
val c = Point3D(0.0, 0.0, 0.0)

// Split line, convert to Point3D and add radius to each coordinate
val data = input.map { line =>
  val temp = line.split(",")
  (temp(0), addRadius( Point3D(temp(1).toDouble, temp(2).toDouble, temp(3).toDouble), radius) )
}

// My starting and ending nodes
val routeStart = addRadius(Point3D(-86.92978121293685, -145.50577813916377, 0), radius)
val routeEnd = addRadius(Point3D(61.678740828293655, 37.37566732303392, 0), radius)

// Append the nodes to data
var dataWithRoute = ("Start", routeStart) +: data :+ ("End", routeEnd)

// String names are pain to work with, convert to Int
val uniqueNames = dataWithRoute.map( i => i._1 ).zipWithIndex.toMap
val dataMap = dataWithRoute.map { i =>
  (uniqueNames(i._1), i._2)
}.toMap

// Adjacency matrix with weights == distances
val ndim = dataWithRoute.size
val AdjacencyMatrix = Array.fill(ndim, ndim)(0.0)
val allnodes = (0 until ndim)
// Only keep the edges that don't intersect with globe
for (i <- allnodes; j <- allnodes) {
  if (i != j) {
    if ( isIntersectingGlobe( dataMap(i), dataMap(j), c, radius ) ) { 
      AdjacencyMatrix(i)(j) = 0.0
    } else {
      val vlen = vectorLength(dataMap(i), dataMap(j))
      AdjacencyMatrix(i)(j) = vlen
      AdjacencyMatrix(j)(i) = vlen
      println("Hit!")
    }
  }
}

// Dijkstra's algorithm


