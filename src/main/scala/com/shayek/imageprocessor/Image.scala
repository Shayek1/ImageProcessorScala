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

  def save(location: String): Unit = {
    ImageIO.write(buffImage,"JPG", new File(location))
  }

  def saveAtResources(location: String): Unit = {
    save(s"src/main/resources/$location")
  }
}

object Image {
  def load(location: String): Image ={
    new Image(ImageIO.read(new File(location)))
  }
  def loadAtResources(location: String): Image ={
    load(s"src/main/resources/$location")
  }
}
