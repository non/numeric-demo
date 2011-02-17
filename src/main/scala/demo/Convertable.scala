package demo

trait Convertable[@specialized A] {
  def toByte(a:A): Byte
  def toShort(a:A): Short
  def toInt(a:A): Int
  def toLong(a:A): Long
  def toFloat(a:A): Float
  def toDouble(a:A): Double

  def fromByte(a:Byte): A
  def fromShort(a:Short): A
  def fromInt(a:Int): A
  def fromLong(a:Long): A
  def fromFloat(a:Float): A
  def fromDouble(a:Double): A
}

object Convertable {
  trait ConvertableByte extends Convertable[Byte] {
    def toByte(a:Byte): Byte = a
    def toShort(a:Byte): Short = a.toShort
    def toInt(a:Byte): Int = a.toInt
    def toLong(a:Byte): Long = a.toLong
    def toFloat(a:Byte): Float = a.toFloat
    def toDouble(a:Byte): Double = a.toDouble

    def fromByte(a:Byte): Byte = a
    def fromShort(a:Short): Byte = a.toByte
    def fromInt(a:Int): Byte = a.toByte
    def fromLong(a:Long): Byte = a.toByte
    def fromFloat(a:Float): Byte = a.toByte
    def fromDouble(a:Double): Byte = a.toByte
  }
  implicit object ConvertableByte extends ConvertableByte

  trait ConvertableShort extends Convertable[Short] {
    def toByte(a:Short): Byte = a.toByte
    def toShort(a:Short): Short = a
    def toInt(a:Short): Int = a.toInt
    def toLong(a:Short): Long = a.toLong
    def toFloat(a:Short): Float = a.toFloat
    def toDouble(a:Short): Double = a.toDouble

    def fromByte(a:Byte): Short = a.toShort
    def fromShort(a:Short): Short = a
    def fromInt(a:Int): Short = a.toShort
    def fromLong(a:Long): Short = a.toShort
    def fromFloat(a:Float): Short = a.toShort
    def fromDouble(a:Double): Short = a.toShort
  }
  implicit object ConvertableShort extends ConvertableShort

  trait ConvertableInt extends Convertable[Int] {
    def toByte(a:Int): Byte = a.toByte
    def toShort(a:Int): Short = a.toShort
    def toInt(a:Int): Int = a
    def toLong(a:Int): Long = a.toLong
    def toFloat(a:Int): Float = a.toFloat
    def toDouble(a:Int): Double = a.toDouble

    def fromByte(a:Byte): Int = a.toInt
    def fromShort(a:Short): Int = a.toInt
    def fromInt(a:Int): Int = a
    def fromLong(a:Long): Int = a.toInt
    def fromFloat(a:Float): Int = a.toInt
    def fromDouble(a:Double): Int = a.toInt
  }
  implicit object ConvertableInt extends ConvertableInt

  trait ConvertableLong extends Convertable[Long] {
    def toByte(a:Long): Byte = a.toByte
    def toShort(a:Long): Short = a.toShort
    def toInt(a:Long): Int = a.toInt
    def toLong(a:Long): Long = a
    def toFloat(a:Long): Float = a.toFloat
    def toDouble(a:Long): Double = a.toDouble

    def fromByte(a:Byte): Long = a.toLong
    def fromShort(a:Short): Long = a.toLong
    def fromInt(a:Int): Long = a.toLong
    def fromLong(a:Long): Long = a
    def fromFloat(a:Float): Long = a.toLong
    def fromDouble(a:Double): Long = a.toLong
  }
  implicit object ConvertableLong extends ConvertableLong

  trait ConvertableFloat extends Convertable[Float] {
    def toByte(a:Float): Byte = a.toByte
    def toShort(a:Float): Short = a.toShort
    def toInt(a:Float): Int = a.toInt
    def toLong(a:Float): Long = a.toLong
    def toFloat(a:Float): Float = a
    def toDouble(a:Float): Double = a.toDouble

    def fromByte(a:Byte): Float = a.toFloat
    def fromShort(a:Short): Float = a.toFloat
    def fromInt(a:Int): Float = a.toFloat
    def fromLong(a:Long): Float = a.toFloat
    def fromFloat(a:Float): Float = a
    def fromDouble(a:Double): Float = a.toFloat
  }
  implicit object ConvertableFloat extends ConvertableFloat

  trait ConvertableDouble extends Convertable[Double] {
    def toByte(a:Double): Byte = a.toByte
    def toShort(a:Double): Short = a.toShort
    def toInt(a:Double): Int = a.toInt
    def toLong(a:Double): Long = a.toLong
    def toFloat(a:Double): Float = a.toFloat
    def toDouble(a:Double): Double = a

    def fromByte(a:Byte): Double = a.toDouble
    def fromShort(a:Short): Double = a.toDouble
    def fromInt(a:Int): Double = a.toDouble
    def fromLong(a:Long): Double = a.toDouble
    def fromFloat(a:Float): Double = a.toDouble
    def fromDouble(a:Double): Double = a
  }
  implicit object ConvertableDouble extends ConvertableDouble
}
