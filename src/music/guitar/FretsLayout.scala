package music.guitar

import quantity.Quantity


case class Fret(fromNut: Quantity) {
  def toBridge(scaleLength: Quantity): Quantity = scaleLength - fromNut
}

case class FretsLayout(items: List[Fret]) {
  def diff(other: FretsLayout): Option[String] = {
    if (items.size != other.items.size) {
      Some("Number of frets differ")
    } else {
      var diffs = List.empty[String]
      for (i <- 0 until items.size) {
        val lhs = items(i)
        val rhs = other.items(i)

        if (lhs.fromNut.roundTo(Scale.dp) != rhs.fromNut.roundTo(Scale.dp)) {
          diffs :+= s"Fret: $i, $lhs != $rhs"
        }
      }
      if (diffs.nonEmpty) Some(diffs.mkString("\n")) else None
    }
  }
}
