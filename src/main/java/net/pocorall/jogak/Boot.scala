package net.pocorall.jogak

import javax.swing.{JTable, JPanel, JLabel, JFrame}
import javax.swing.table.TableModel

class ServiceDiscovery {

}


trait TreeNodeModel {
  def parent(): TreeNodeModel

  def child(): Array[TreeNodeModel]
}

trait TreeModel[E <: TreeNodeModel] {
  def root(): E
}

class File(file: java.io.File = new java.io.File("/")) extends TreeNodeModel {
  def parent() = new File(file.getParentFile)

  def child() = {

    new File(file.getParentFile)
  }

}

class FileSystemModel extends TreeModel[File] {

}


class TreeViewer[E] extends JPanel {
  var table = new JTable;
  add(table);
  var model: TreeModel[E];

  def setModel(model: TreeModel[E]) {
    this.model = model;
    table.setModel(new TableModel() {

    });
  }

}

object Boot {
  def showDesktop() {
    javax.swing.SwingUtilities.invokeLater(new Runnable() {
      def run() {
        val frame = new JFrame("HelloWorldSwing");
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

        val label = new TreeViewer();
        frame.getContentPane().add(label);

        frame.pack();
        frame.setVisible(true);
      }
    });
  }

  def main(args: Array[String]) {
    showDesktop();
  }
}