package music


trait GuitarTuning {
  def openStrings: List[Note]
}

case object StandardTuning extends GuitarTuning {
  override def openStrings: List[Note] = List("E4", "B3", "G3", "D3", "A2", "E2").map(Note(_))
}
