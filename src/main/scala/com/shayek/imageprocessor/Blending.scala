package com.shayek.imageprocessor

trait Blending {
  def combine(front: Pixels, back: Pixels): Pixels
}

//Transparency formula - (opacity * front layer + back layer * (1 - opacity))
class Transparency(t: Double) extends Blending {
  private val opacity = {
    if t <= 0 then 0.0
    else if t >= 1.0 then 1.0
    else t
  }

  override def combine(front: Pixels, back: Pixels): Pixels = {
    Pixels(
      (opacity * front.r + back.r * (1-opacity)).toInt,
      (opacity * front.g + back.g * (1-opacity)).toInt,
      (opacity * front.b + back.b * (1-opacity)).toInt,
    )
  }
}

//multiply formula - front layer * back layer
//have to convert the pixel value so it does not exceed 255 after multiplying
object Multiply extends Blending {

  override def combine(front: Pixels, back: Pixels) =
    Pixels(
      ((front.r * back.r)/255.0).toInt,
      ((front.g * back.g)/255.0).toInt,
      ((front.b * back.b)/255.0).toInt,
    )
}

//screen formula - front layer + back layer - ( front layer * back layer)
//into pixel value - front layer + back layer - ((front layer * back layer)/255)
object Screen extends Blending{

  override def combine(front: Pixels, back: Pixels): Pixels =
    Pixels(
      (front.r + back.r - ((front.r - back.r)/255)).toInt,
      (front.g + back.g - ((front.g - back.g)/255)).toInt,
      (front.b + back.b - ((front.b - back.b)/255)).toInt,
    )

}
