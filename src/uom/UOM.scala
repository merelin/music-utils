package uom


object UOMType extends Enumeration {
  type UOMType = Value
  val Length = Value("Length")
  val Mass = Value("Mass")
  val Volume = Value("Volume")
  val Time = Value("Time")
  val Bytes = Value("Bytes")
  val Pressure = Value("Pressure")
  val Currencies = Value("Currencies")
  val Temperature = Value("Temperature")
  val Percent = Value("Percent")
}

object UOMSymbol extends Enumeration {
  protected case class UOMSymbolVal(i: Int, longName: String, shortName: String, symbols: List[String]) extends super.Val(i, longName) {
    override def toString: String = shortName
  }

  type UOMSymbol = Value
  val Meter = UOMSymbolVal(0, "Meter", "m", List("meters", "meter", "metres", "metre", "m"))
  val Decimeter = UOMSymbolVal(1, "Decimeter", "dm", List("decimeters", "decimeter", "decimetres", "decimetre", "dm"))
  val Centimeter = UOMSymbolVal(2, "Centimeter", "cm", List("centimeters", "centimeter", "centimetres", "centimetre", "cm"))
  val Millimeter = UOMSymbolVal(3, "Millimeter", "mm", List("millmeters", "millmeter", "millmetres", "millmetre", "mm"))
  val Yard = UOMSymbolVal(4, "Yard", "yd", List("yards", "yard", "yd"))
  val Foot = UOMSymbolVal(5, "Foot", "ft", List("foots", "foot", "ft"))
  val Inch = UOMSymbolVal(6, "Inch", "in", List("inches", "inch", "in"))
  val Chain = UOMSymbolVal(7, "Chain", "ch", List("chains", "chain", "ch"))
  val Furlong = UOMSymbolVal(8, "Furlong", "furlong", List("furlongs", "furlong"))
  val Mile = UOMSymbolVal(9, "Mile", "mi", List("miles", "mile", "mi", "m"))

  val Percent = UOMSymbolVal(10, "Percent", "%", List("percents", "percent", "%"))
  val Scalar = UOMSymbolVal(11, "Scalar", "", List("scalar"))
}

trait UOM {
  def uomType: UOMType.UOMType
  def uomSymbol: UOMSymbol.UOMSymbol
  def conversionFactor(targetUom: UOM): Option[BigDecimal]

  override def toString: String = uomSymbol.toString
}

case class Pair(from: BaseUOM, to: BaseUOM)

object BaseUOM {
  val conversionFactors = Map(
    Pair(Yard, Meter) -> BigDecimal("0.9144"),
    Pair(Meter, Yard) -> UOM.One / BigDecimal("0.9144")
  )

  def conversionFactorsFor(uom: BaseUOM): Map[BaseUOM, BigDecimal] = {
    conversionFactors.collect { case (pair, factor) if pair.from == uom => pair.to -> factor }
  }
}

class BaseUOM(val uomType: UOMType.UOMType,
              val uomSymbol: UOMSymbol.UOMSymbol
             ) extends UOM {

  override def conversionFactor(targetUom: UOM): Option[BigDecimal] = {
    targetUom match {
      case bu: BaseUOM if this == bu => Some(UOM.One)
      case bu: BaseUOM => BaseUOM.conversionFactorsFor(this).get(bu)
      case du: DerivativeUOM => conversionFactor(du.baseUom).map(c => c / du.baseUomConversionFactor)
      case _ => throw new IllegalArgumentException(s"Unsupported target uom: ${targetUom.uomSymbol}")
    }
  }
}

class DerivativeUOM(val uomType: UOMType.UOMType,
                    val uomSymbol: UOMSymbol.UOMSymbol,
                    val baseUom: BaseUOM,
                    val baseUomConversionFactor: BigDecimal
                   ) extends UOM {
  override def conversionFactor(targetUom: UOM): Option[BigDecimal] = {
    targetUom match {
      case bu: BaseUOM => baseUom.conversionFactor(bu).map(c => baseUomConversionFactor * c)
      case du: DerivativeUOM if this == du => Some(UOM.One)
      case du: DerivativeUOM => baseUom.conversionFactor(du).map(c => baseUomConversionFactor * c)
      case _ => throw new IllegalArgumentException(s"Unsupported target uom: ${targetUom.uomSymbol}")
    }
  }
}

case object Meter extends BaseUOM(UOMType.Length, UOMSymbol.Meter)

case object Decimeter extends DerivativeUOM(UOMType.Length, UOMSymbol.Decimeter, Meter, BigDecimal("0.1"))

case object Centimeter extends DerivativeUOM(UOMType.Length, UOMSymbol.Centimeter, Meter, BigDecimal("0.01"))

case object Millimeter extends DerivativeUOM(UOMType.Length, UOMSymbol.Millimeter, Meter, BigDecimal("0.001"))

case object Yard extends BaseUOM(UOMType.Length, UOMSymbol.Yard)

case object Foot extends DerivativeUOM(UOMType.Length, UOMSymbol.Foot, Yard, UOM.One / BigDecimal("3.0"))

case object Inch extends DerivativeUOM(UOMType.Length, UOMSymbol.Inch, Yard, UOM.One / BigDecimal("36.0"))

case object Chain extends DerivativeUOM(UOMType.Length, UOMSymbol.Chain, Yard, BigDecimal("22.0"))

case object Furlong extends DerivativeUOM(UOMType.Length, UOMSymbol.Furlong, Yard, BigDecimal("220.0"))

case object Mile extends DerivativeUOM(UOMType.Length, UOMSymbol.Mile, Yard, BigDecimal("1760.0"))

object UOM {
  val One = BigDecimal("1.0")

  def main(args: Array[String]): Unit = {
    Yard.conversionFactor(Yard).foreach { v => println(f"${Yard.uomSymbol} = ${v}%4.4f * ${Yard.uomSymbol}") }
    Yard.conversionFactor(Inch).foreach { v => println(f"${Yard.uomSymbol} = ${v}%4.4f * ${Inch.uomSymbol}") }
    Foot.conversionFactor(Inch).foreach { v => println(f"${Foot.uomSymbol} = ${v}%4.4f * ${Inch.uomSymbol}") }
    Inch.conversionFactor(Decimeter).foreach { v => println(f"${Inch.uomSymbol} = ${v}%4.4f * ${Decimeter.uomSymbol}") }
    Inch.conversionFactor(Centimeter).foreach { v => println(f"${Inch.uomSymbol} = ${v}%4.4f * ${Centimeter.uomSymbol}") }
    Inch.conversionFactor(Millimeter).foreach { v => println(f"${Inch.uomSymbol} = ${v}%4.4f * ${Millimeter.uomSymbol}") }
  }
}