import sbt._

class Project(i:ProjectInfo) extends DefaultProject(i) {
  override def compileOptions = {
    CompileOption("-optimise") :: super.compileOptions.toList
  }

  override def testCompileOptions = {
    CompileOption("-optimise") :: super.compileOptions.toList
  }
}
