package com.shayek.imageprocessor

//setting the base of the trait
trait Transform {
  def apply(image: Image): Image
}

case class Crop(x: Int, y: Int, w: Int, h:Int) extends Transform{
  override def apply(image: Image): Image =
    try{
      image.crop(x,y,w,h)
    } catch{
      case _: Exception =>
        println(s"Error: Coordinates cannot go beyond ${image.width}, ${image.height}")
        image
    }
}

case object Invert extends Transform{
  override def apply(image: Image): Image =
    image.transformMap{pixel => 
      Pixels(
        255 - pixel.r, 255 - pixel.g, 255 - pixel.b
      )}
}

case object Greyscale extends Transform {
  override def apply(image: Image): Image = {
    image.transformMap { pixel =>
      val makingGrey = (pixel.r + pixel.g + pixel.b) / 3
      Pixels(makingGrey, makingGrey, makingGrey)
    }
  }

}

object Transform {
  def main(args: Array[String]): Unit = {
    
  }
}