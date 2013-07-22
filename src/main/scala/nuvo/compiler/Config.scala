package nuvo.compiler

object Config {
  val helpMessage =
    """
      nuvoc [opts] <file-name>

      Available options are:

          -h: shows this help
          -d: produce the jar with all dependencies for the project
          -o: output directory


      Example 1: To compile a file you can use the following command:

         nuvoc path/to/my/file.scala



      Example 2: To generate the jar with the dependencies for a project

          nuvoc -d myapp-deps.jar

      Notice that this command has to be executed from the root of the sbt project.

    """.stripMargin

  val nuvocSourcePath = "src/main/nuvoc"

  val helperSuffix = "Helper"
  val binExtension = ".class"
  val srcExtension = ".scala"
  val targetDir = "./target"
  val nuvocDir = "./src/main/nuvoc"
  val generatedDir = "./src/main/generated"
  val deployDir = "deploy"
  val anonfunTag = "anonfun"
  val classDir = "classes"
  val depfile = "depjar.sh"
  val jarcmd = "jar"
  val jaropt = "cf"
  val jarDirOpt = "-C"
  val compileAllNuvoCFilesOpt = "-a"



  val tupleType = "nuvo.core.Tuple"
}
