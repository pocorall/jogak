package net.pocorall.jogak.viewer

import net.pocorall.jogak.Viewer
import javax.swing.{JLabel, JScrollPane, JTextArea}
import java.awt.image.BufferedImage
import java.awt.Graphics

class SimpleStringViewer(str: String) extends Viewer {
  private val textArea = new JTextArea(str)
  textArea.setEditable(false)
  add(new JScrollPane(textArea))
}

class EverythingViewer(thing: Any) extends Viewer {
  add(new JLabel(thing.toString))
}

class SimpleImageViewer(img: BufferedImage) extends Viewer {
  override def paintComponent(g: Graphics) {
    super.paintComponent(g)
    g.drawImage(img, 0, 0, null)
  }
}