package playground.language

object FileNames {

  def isOutputPanel(
    fileName: String
  ): Boolean = fileName.startsWith("output:")

}
