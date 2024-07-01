package music.guitar

import uom._


object FretsLayoutCalculator {

  def main(args: Array[String]): Unit = {
    val uom = Inch
//    val uom = Millimeter

    MartinOMScale.fretsLayout(uom).diff(MartinOMScale.fretsLayoutOverride(uom)).foreach(diff => println(s"\nMartinOM:\n$diff"))
    MartinOOOScale.fretsLayout(uom).diff(MartinOOOScale.fretsLayoutOverride(uom)).foreach(diff => println(s"\nMartinOOO:\n$diff"))
  }
}
