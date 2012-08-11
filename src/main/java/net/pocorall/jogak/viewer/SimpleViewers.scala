package net.pocorall.jogak.viewer

import net.pocorall.jogak.{SplitViewer, Viewer}
import javax.swing._
import java.awt.image.BufferedImage
import java.awt.{BorderLayout, Graphics}
import java.awt.event.{MouseEvent, MouseAdapter}

class SimpleStringViewer(str: String) extends Viewer {
  override def thing = str

  private val textArea = new JTextArea(str)
  textArea.setEditable(false)
  addContextMenuListener(textArea)

  add(new JScrollPane(textArea), BorderLayout.CENTER)
}

class EverythingViewer(obj: Any) extends Viewer {
  override def thing = obj

  addContextMenuListener(this)
  add(new JLabel("Anything: " + obj.toString))
}

class SimpleImageViewer(img: BufferedImage) extends Viewer {
  override def thing = img

  addContextMenuListener(this)

  override def paintComponent(g: Graphics) {
    super.paintComponent(g)
    g.drawImage(img, 0, 0, null)
  }
}

class ListViewer(lst: List[Any]) extends SplitViewer {
  override def thing = lst

  val selfButton = new JButton("Self")
  leftPane.add(new JScrollPane(selfButton), BorderLayout.NORTH)
  addContextMenuListener(selfButton)


  val listComponent = new JList(lst.toArray.asInstanceOf[Array[Object]])
  leftPane.add(new JScrollPane(listComponent), BorderLayout.CENTER)

  listComponent.addMouseListener(new MouseAdapter() {
    override def mouseClicked(e: MouseEvent) {
      val p = e.getPoint

      val row = listComponent.locationToIndex(p)
      if (row < 0) return
      val newModel = lst(row)

      if (SwingUtilities.isRightMouseButton(e)) {
        showContextMenu(e.getComponent, e.getX, e.getY, newModel)
      }
    }
  })
}