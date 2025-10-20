package com.shayek.imageprocessor

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

case class Image private(private val buffImage: BufferedImage) {
  val height = buffImage.getHeight
  val width = buffImage.getWidth

  def getColour(xColumn: Int, yRow: Int): Pixels = {
    Pixels.hexForm(buffImage.getRGB(xColumn, yRow))
  }

  def setColour(xRow: Int, yRow: Int, p: Pixels): Unit  = {
    buffImage.setRGB(xRow, yRow, p.intForm)
  }

  def crop(firstX: Int, firstY: Int, w: Int, h: Int): Image = {
    assert(
      firstX >= 0 && 
        firstY >= 0 && 
        w > 0 && h > 0 &&
        firstX + w < width && firstY + h < height
    )
    val newPixels = Array.fill(h * w)(0)
    buffImage.getRGB(firstX, firstY, w, h, newPixels, 0,w)
    val newBuffImage = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB)
    newBuffImage.setRGB(0, 0, w, h, newPixels, 0, w)
    new Image(newBuffImage)
  }

  def save(location: String): Unit = {
    ImageIO.write(buffImage,"JPG", new File(location))
  }

  def saveAtResources(location: String): Unit = {
    save(s"src/main/resources/$location")
  }
  
  def transformMap(function: Pixels => Pixels): Image = {
    val newPixels  = Array.fill(height * width)(0)
    buffImage.getRGB(0,0,width,height, newPixels, 0, width)
    newPixels.mapInPlace{ colour =>
      val pixel = Pixels.hexForm(colour)
      val newPixel = function(pixel)
      newPixel.intForm
    }
    
    val newBuffImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    newBuffImage.setRGB(0,0,width, height,newPixels, 0, width)
    new Image(newBuffImage)
  }
}

object Image {
  def load(location: String): Image ={
    new Image(ImageIO.read(new File(location)))
  }
  def loadAtResources(location: String): Image ={
    load(s"src/main/resources/$location")
  }

  def main(args: Array[String]): Unit = {

    loadAtResources("city.jpg").crop(300, 200, 100, 100).saveAtResources("cityCropped.jpg")
  }
}
