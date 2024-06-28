package music.guitar

import music.TemperedTuning


case class FretInfo(toNut: Double, toBridge: Double, width: Double)

case class FretsLayout(items: List[FretInfo])

object FretsLayout {
  // Use more precision than 'magical 17.817'
  val offsetMultiplier: Double = TemperedTuning.multiplier / (TemperedTuning.multiplier - 1)

  val inchInMm = 25.4

  // Please mind that this is a theoretical layout without compensation
  def apply(scaleLength: Double, numberOfFrets: Int): FretsLayout = {
    var items: List[FretInfo] = List(FretInfo(0.0, scaleLength, 0.0))

    for (i <- 1 to numberOfFrets) {
      val prevItem = items(i - 1)
      val toNut = prevItem.toNut + (prevItem.toBridge / offsetMultiplier)
      val toBridge = scaleLength - toNut
      val width = toNut - prevItem.toNut
      items :+= FretInfo(toNut, toBridge, width)
    }

    FretsLayout(items)
  }
}

object FretsPositionsCalculator {

  def main(args: Array[String]): Unit = {
    val scaleLengthInInches = 25.5
    val scaleLengthInMillimeters = 645.0 //scaleLengthInInches * inchInMm

    val layout = FretsLayout(scaleLengthInMillimeters, 24)

    println(f"Scale: ${scaleLengthInMillimeters}%4.3f")

    for (i <- 0 to 24) {
      val item = layout.items(i)
      println(f"${i}%2d\t${item.toNut}%4.3f\t${item.toBridge}%4.3f\t${item.width}%4.3f")
    }
  }
}