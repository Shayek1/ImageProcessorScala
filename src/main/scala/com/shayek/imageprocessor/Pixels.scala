package com.shayek.imageprocessor

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

case class Pixels(r: Int, g: Int, b: Int) {
  //to make sure red/green and blue are within 0 - 255
  assert(r >= 0 && r < 256)
  assert(g >= 0 && g < 256)
  assert(b >= 0 && b < 256)

  def intForm: Int = {
    (r << 16) | (g << 8) | b
  } //this is to convert the colour into its hex form

  def drawImage(width: Int, height: Int, path: String) = {
    val colour = intForm
    val image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    val pixels = Array.fill(width * height)(colour)
    image.setRGB(0,0,width,height,pixels,0,width)
    ImageIO.write(image,"JPG", new File(path))
  } //takes the values to make the image

  infix def +(another: Pixels): Pixels = {
    Pixels(
      Pixels.limiter(r + another.r),
      Pixels.limiter(g + another.g),
      Pixels.limiter(b + another.b)
    )
  }
}

object Pixels {
  val RED = Pixels(255, 0, 0)
  val GREEN = Pixels(0, 255, 0)
  val BLUE = Pixels(0, 0, 255)
  val WHITE = Pixels(255, 255, 255)
  val BLACK = Pixels(0, 0, 0)
  val GREY = Pixels(128, 128, 128)
  val PINK = Pixels(255, 192, 203)
  val YELLOW = Pixels(255, 255, 0)

  def limiter(limiter: Int): Int = {
    if limiter <= 0 then 0
    else if limiter > 256 then 255
    else limiter
  } //this will make sure the new pixel value stays within 0-255 range

  def main(args: Array[String]): Unit = {
    val purple = RED + BLUE
    purple.drawImage(100, 50, "src/main/resources/Pixels/anotherPurple.jpg")

  }
}
