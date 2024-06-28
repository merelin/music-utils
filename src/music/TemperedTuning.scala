package music


object TemperedTuning {
  val multiplier: Double = Math.pow(2.0, (1.0 / 12.0))

  def multiplierFor(numberOfHalfTones: Int): Double = Math.pow(2.0, (numberOfHalfTones * 1.0 / 12.0))
}
