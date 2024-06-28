package music


case class Note(note: String, octave: Int, frequency: Double) {
  override def toString: String = f"$note$octave == $frequency%4.2f Hz"
}

object Note {

  val supportedNotes = Set('C', 'D', 'E', 'F', 'G', 'A', 'B')
  val supportedModifiers = Set("-flat", "b", "-sharp", "#")
  val supportedOctaves = ('1' to '6').map(i => i).toSet

  def apply(s: String): Note = {
    val notePart = s.charAt(0)
    val modifierPart = s.substring(1, s.length - 1)
    val octavePart: Character = s.charAt(s.length - 1)

    require(supportedNotes.contains(notePart), s"Unsupported note: ${notePart}")
    require(modifierPart.isEmpty || supportedModifiers.contains(modifierPart), s"Unsupported modifier: ${modifierPart}")
    require(supportedOctaves.contains(octavePart), s"Unsupported octave: ${octavePart}")

    val octave = octavePart - '0'
    val note = modifierPart match {
      case "-flat" | "b" => s"${notePart}b"
      case "-sharp" | "#" => s"${notePart}#"
      case _ => s"${notePart}"
    }

    Note(note, octave, frequencyFor(note, octave))
  }

  def frequencyOfA(octave: Int): Double = octave match {
    case 1 => 55.00
    case 2 => 110.00
    case 3 => 220.00
    case 4 => 440.00
    case 5 => 880.00
    case 6 => 1760.00
    case _ => throw new IllegalArgumentException(s"Unsupported octave: ${octave}")
  }

  def frequencyFor(note: String, octave: Int): Double = {
    val freqA = frequencyOfA(octave)
    val numberOfHalfTones = note match {
      case "C" => -9
      case "C#" | "Db" => -8
      case "D" => -7
      case "D#" | "Eb" => -6
      case "E" => -5
      case "F" => -4
      case "F#" | "Gb" => -3
      case "G" => -2
      case "G#" | "Ab" => -1
      case "A" => 0
      case "A#" | "Bb" => 1
      case "B" => 2
    }
    val frequency = freqA * TemperedTuning.multiplierFor(numberOfHalfTones)
    frequency
  }
}