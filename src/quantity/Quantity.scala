package quantity

import uom._

import scala.math.BigDecimal.RoundingMode


case class Quantity(value: BigDecimal, uom: UOM) {

  override def toString: String = f"${value}%4.4f ${uom}"

  def op(other: Quantity)(fn: (BigDecimal, BigDecimal) => BigDecimal): Quantity = {
    other.uom match {
      case `uom` => Quantity(fn(value, other.value), uom)
      case ou => ou.conversionFactor(uom) match {
        case Some(cf) => Quantity(fn(value, other.value * cf), uom)
        case _ => throw new IllegalArgumentException(
          s"Operation on two quantities with different units (${uom}, ${other.uom}) is unsupported"
        )
      }
    }
  }

  def +(other: Quantity): Quantity = {
    op(other) { case (lhs, rhs) => lhs + rhs }
  }

  def -(other: Quantity): Quantity = {
    op(other) { case (lhs, rhs) => lhs - rhs }
  }

  def *(other: Quantity): Quantity = {
    op(other) { case (lhs, rhs) => lhs * rhs }
  }

  def /(other: Quantity): Quantity = {
    op(other) { case (lhs, rhs) => lhs / rhs }
  }

  def *(factor: BigDecimal): Quantity = Quantity(value * factor, uom)
  def /(factor: BigDecimal): Quantity = Quantity(value / factor, uom)

  def zero: Quantity = Quantity.zero(uom)
  def one: Quantity = Quantity.one(uom)

  def inUOM(targetUom: UOM): Option[Quantity] = {
    uom.conversionFactor(targetUom).map { cf => Quantity(value * cf, targetUom) }
  }

  def almostEquals(other: Quantity, dp: Int): Boolean = {
    uom == other.uom && (value - other.value).abs < Quantity.tolerance(dp)
  }

  def roundTo(dp: Int): Quantity = {
    Quantity(value.setScale(dp, RoundingMode.HALF_UP), uom)
  }
}

object Quantity {
  def zero(uom: UOM): Quantity = Quantity(BigDecimal("0.0"), uom)
  def one(uom: UOM): Quantity = Quantity(BigDecimal("1.0"), uom)

  def tolerance(dp: Int): BigDecimal = BigDecimal("10.0").pow(-dp)
}
