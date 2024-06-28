package music

import javax.sound.sampled.{AudioFormat, AudioSystem, SourceDataLine}


object Tone {
  val SampleRate: Double = 8000.0

  def sound(frequency: Double, ms: Int, volume: Double): Unit = {
    val buf: Array[Byte] = new Array[Byte]((SampleRate * ms / 1000).toInt)

    for (i <- 0 until buf.length) {
      val angle = i / (SampleRate / frequency) * 2.0 * Math.PI
      buf(i) = (Math.sin(angle) * 127.0 * volume).toByte
    }

    // shape the front and back 10ms of the wave form
    var i = 0
    while (i < (SampleRate / 100.0) && i < (buf.length / 2)) {
      buf(i) = (buf(i) * i / (SampleRate / 100.0)).toByte
      buf(buf.length - 1 - i) = (buf(buf.length - 1 - i) * i / (SampleRate / 100.0)).toByte
      i += 1
    }

    val af = new AudioFormat(SampleRate.toFloat, 8, 1, true, false)
    val sdl = AudioSystem.getSourceDataLine(af)
    sdl.open(af)
    sdl.start()
    sdl.write(buf, 0, buf.length)
    sdl.drain()
    sdl.close()
  }
}

object Player {

  def play(line: SourceDataLine, frequency: Double, durationMs: Int): Unit = {
    val sampleRate = 16 * 1024
    val length = sampleRate * durationMs / 1000
    val data: Array[Byte] = new Array[Byte](sampleRate)
    for (i <- 0 until length) {
      val period: Double = sampleRate / frequency
      val angle: Double = 2.0 * Math.PI * i / period
      data(i) = (Math.sin(angle) * 127).toByte
    }
    val count = line.write(data, 0, length)
  }

  def main(args: Array[String]): Unit = {
    val sampleRate = 16 * 1024
    val af = new AudioFormat(sampleRate, 8, 1, true, true)
    val line = AudioSystem.getSourceDataLine(af)
    line.open(af, sampleRate)
    line.start()

    List("C3", "D3", "E3", "F3", "G3", "A3", "B3", "C4").map(n => Note(n)).foreach { n =>
      play(line, n.frequency, 1000)
//      play(line, 0, 10)
    }
    line.drain()
    line.close()

//    List("C3", "D3", "E3", "F3", "G3", "A3", "B3", "C4").map(Note(_)).foreach { n => Tone.sound(n.frequency, 500, 1.0) }
  }
}