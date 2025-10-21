package com.shayek.imageprocessor

//setting the base of the trait
trait Transform {
  def apply(image: Image): Image
}
//Returns the original image if the user inputs coordinates outside the maximum capacity.
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

//Invert - produces the inverted colours of the image
case object Invert extends Transform{
  override def apply(image: Image): Image =
    image.transformMap{pixel =>
      Pixels(
        255 - pixel.r, 255 - pixel.g, 255 - pixel.b
      )}
}

//Greyscale - makes the image grey
case object Greyscale extends Transform {
  override def apply(image: Image): Image = {
    image.transformMap { pixel =>
      val makingGrey = (pixel.r + pixel.g + pixel.b) / 3
      Pixels(makingGrey, makingGrey, makingGrey)
    }
  }

//Colourise - adds a colour filter to the image
//First greyscaling the image, then adding a colour of the users choice on top.
  case class Colourise(colour: Pixels) extends Transform{
    override def apply(image: Image): Image = {
      image.transformMap{pixel =>
        val factor = (pixel.r + pixel.g + pixel.b) / 3
        Pixels(
        (colour.r * (factor/255.0)).toInt,
        (colour.g * (factor/255.0)).toInt,
        (colour.b * (factor/255.0)).toInt,
      )}
    }
  }

}

object Transform {
  def main(args: Array[String]): Unit = {
    //Invert(Image.loadAtResources("nature.jpg")).saveAtResources("natureInverted.jpg")
    //Greyscale(Image.loadAtResources("nature.jpg")).saveAtResources("natureGrey.jpg")
  }
}