package music.guitar

import music.TemperedTuning
import quantity.Quantity
import uom.{Inch, Millimeter, UOM}


object Scale {
  // Use a bit more precision than 'magical 17.817'
  val factor: Double = TemperedTuning.factor / (TemperedTuning.factor - 1)
  // Number of decimal places
  val dp: Int = 4
}

class Scale(val length: Quantity, val numberOfFrets: Int) {

  def lengthIn(uom: UOM): Quantity = {
    length.uom match {
      case `uom` => length
      case _ => length.inUOM(uom)
        .getOrElse(throw new IllegalArgumentException(s"Scale length cannot be converted from ${length.uom} to ${uom}"))
    }
  }

  // Please mind that this is a theoretical layout without compensation
  def fretsLayout(uom: UOM): FretsLayout = {
    val lengthInUOM = lengthIn(uom)
    val zero = lengthInUOM.zero
    var items: List[Fret] = List(Fret(zero))

    for (i <- 1 to numberOfFrets) {
      val prevItem = items(i - 1)
      val fromNut = prevItem.fromNut + ((lengthInUOM - prevItem.fromNut) / Scale.factor)
      items :+= Fret(fromNut)
    }

    FretsLayout(items)
  }

  // This is a manufacturer layout in different UOMs. May not correspond to converted values (from UOM to UOM).
  def fretsLayoutOverride: Map[UOM, FretsLayout] = Map.empty[UOM, FretsLayout]

  def buildLayoutForOverride(uom: UOM, values: List[String]): FretsLayout = {
    require(values.size == numberOfFrets, s"Number of frets ${numberOfFrets} does not match override ${values.size}")

    val lengthInUOM = lengthIn(uom)
    val zero = lengthInUOM.zero
    var items: List[Fret] = List(Fret(zero))

    for (i <- 0 until numberOfFrets) yield {
      val fromNut = Quantity(BigDecimal(values(i)), uom)
      items :+= Fret(fromNut)
    }

    FretsLayout(items)
  }
}

case object MartinOMScale extends Scale(Quantity(BigDecimal("25.400"), Inch), 20) {
  override def fretsLayoutOverride: Map[UOM, FretsLayout] = Map(
    Inch -> List(
      "1.4256", "2.7712", "4.0412", "5.2400", "6.3715", "7.4395", "8.4475", "9.3990", "10.2971", "11.1447",
      "11.9448", "12.7000", "13.4128", "14.0856", "14.7206", "15.3200", "15.8860", "16.4198", "16.9238", "17.3995"
    ),
    Millimeter -> List(
      "36.2102", "70.3877", "102.6475", "133.0957", "161.8358", "188.9628", "214.5673", "238.7343", "261.5453", "283.0761",
      "303.3982", "322.5800", "340.6851", "357.7742", "373.9037", "389.1283", "403.5036", "417.0617", "429.8638", "441.9476"
    )
  ).map { case (u, l) => u -> buildLayoutForOverride(u, l) }
}

case object MartinOOOScale extends Scale(Quantity(BigDecimal("24.900"), Inch), 20) {
  override def fretsLayoutOverride: Map[UOM, FretsLayout] = Map(
    Inch -> List(
      "1.3975", "2.7166", "3.9617", "5.1368", "6.2461", "7.2930", "8.2812", "9.2140", "10.0944", "10.9253",
      "11.7097", "12.4500", "13.1488", "13.8083", "14.4308", "15.0184", "15.5733", "16.0965", "16.5906", "17.0570"
    ),
    Millimeter -> List(
      "35.497", "69.002", "100.627", "130.476", "158.650", "185.243", "210.343", "234.035", "256.397", "277.504",
      "297.426", "316.230", "333.979", "350.731", "366.543", "381.468", "395.561", "408.852", "421.402", "433.248"
    )
  ).map { case (u, l) => u -> buildLayoutForOverride(u, l) }
}
