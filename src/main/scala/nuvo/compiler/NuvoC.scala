package nuvo.compiler

import java.io.{FileReader, File, PrintStream}
import scala.tools.nsc.util.ScalaClassLoader.URLClassLoader

/**
 * NuvoC compiler
 */
object NuvoC extends Parser {
  def printTranslationUnit(stream: PrintStream)(idt: String)(tu: List[GeneratedCode]) {
    tu foreach {
      case Line(s) => stream.println(s)
      case Block(b)  => b foreach {
        case Line(s) => stream.println(idt + s)
        case nb: Block => printGenerateCode(stream)(idt)(nb)
      }
    }
  }
  def printGenerateCode(stream: PrintStream)(indentation: String)(code: GeneratedCode) {
    def loop(idt: String, block: GeneratedCode) {
      block match {
        case Line(s) => stream.println(idt + s)
        case Block(l) => l.foreach(loop(idt + indentation, _))
      }
    }
    loop("  ", code)
  }

  sealed abstract class Command
  case class Compile(files: List[String], outPath: String) extends Command
  case class Dependency(file: List[String], jarfile: String) extends Command
  case object Help extends Command

  def recursivelyListFiles(path: String, ext: String): List[String] = {
    val rootPath = new File(path)
    val dirlist = rootPath.list().toList
    val files = dirlist.filter(_.contains(ext))

    val dirs = (rootPath.listFiles().filter(_.isDirectory).map(_.getName)).toList

    files.map (path + File.separator + _) ::: dirs.flatMap(d => recursivelyListFiles(path + File.separator +  d, ext))

  }

  def parseArgs(args: Array[String]): Command = {
    val files = args.filter(_.contains(Config.srcExtension)).toList
    val idx = args.indexOf("-o")
    val outpath = if (idx != -1) args(idx + 1) else System.getProperty("user.dir")

    if (args.length < 1) Help
    else
      args.find(_ == "-h").map(x => Help).getOrElse {
        args.find(_ == "-d").map(x => {
          if (args.length > 1) {
            val idx = args.indexOf("-d")
            val path = System.getProperty("user.dir") + File.separator + Config.nuvocSourcePath
            val jarfile = args(idx + 1)
            val nuvocFiles = recursivelyListFiles(path, Config.srcExtension)
            Dependency(nuvocFiles, jarfile)
          } else Help
        }).getOrElse {
          Compile(files, outpath)
        }
      }
  }

  def doCommand(filename: String, outpath: String, extension: String, cmd: (TranslationUnit, PrintStream) => Unit, createNSDirs: Boolean = true) {
    val freader = new FileReader(filename)

    // Parse Input
    val nt = parseAll(root, freader)
    freader.close()

    // Print error if necessary
    if (!nt.successful)
      println(nt)

    // Generate Code
    nt map { t =>
      val path =
        if (createNSDirs) outpath + "/" + t.pkg.map(_.replace(".", "/")).getOrElse("")
        else outpath

      val dir = new File(path)
      dir.mkdirs()

      val fqpl = filename.split(File.separatorChar)

      val ofile = new File(path + File.separatorChar + fqpl.last.takeWhile(_ != '.' ) + extension)
      val dataout = new PrintStream(ofile)

      cmd(t, dataout)

      dataout.flush()
      dataout.close()
    }
  }
  def doCompile(filename: String, outpath: String) {
    val cmd = (t: TranslationUnit, dataout: PrintStream) => {
      val genc = Generator.NDRGenerator.generateTranslationUnit(t)
      printTranslationUnit(dataout)("  ")(genc)
    }
    doCommand(filename, outpath, Config.helperSuffix + Config.srcExtension, cmd)
  }

  def extractDefinedTypes(filename: String): List[String] = {
    val freader = new FileReader(filename)

    // Parse Input
    val nt = parseAll(root, freader)
    freader.close()

    // Print error if necessary
    if (!nt.successful)
      List()
    else {
      val t = nt.get
      t.types.filterNot(_.isAbstract).map {
        nt => nt.pkg + "." + nt.name
      }
    }
  }

  def doDependency(filename: String, outpath: String) {
    val cmd = (t: TranslationUnit, dataout: PrintStream) => {
      val types = t.types.map {
        nt => nt.pkg + "." + nt.name + "\n"
      }
      types.foreach(dataout.print(_))
    }
    doCommand(filename, outpath, ".ntl", cmd, false)
  }

  def resolveClassDir(): Option[String] = {
    val rootDir = new File(Config.targetDir)
    rootDir.list().find(_.contains("scala")).map(Config.targetDir + File.separator + _ + File.separator + Config.classDir )
  }

  def resolveAnonfunDependencies(nt: List[String]) : List[String] = {
    val types = Config.tupleType :: nt

    resolveClassDir() match {
      case None => List()
      case Some(s) => {
        val f = new File(s)

        val url = f.toURI.toURL
        val ucl = new URLClassLoader(List(url), Thread.currentThread.getContextClassLoader)

        val files = recursivelyListFiles(s, Config.binExtension)
        val afuns = files.filter(_.contains(Config.anonfunTag)).map(_.drop(s.length + 1))

        val file2type = (f: String) => {
          f.takeWhile(_ != '.').replace(File.separatorChar, '.')
        }
        val types = afuns map (file2type(_))

        val isNuvoTypeLambda = (t: String) => {
          try {
            val cls = ucl.loadClass(t)
            val applys = cls.getMethods.filter(_.getName == "apply")
            val check = applys.map { m => {
              val paramTypes = m.getParameterTypes
              if (paramTypes.length != 1) false
              else {
                types.exists(_ == paramTypes(0).getCanonicalName)
              }
            }}
            check.fold(true)(_ || _)
          } catch {
            case cnfe: ClassNotFoundException => false
            case ncdfe: NoClassDefFoundError => false
          }
        }
        val nuvoAfuns = types filter(isNuvoTypeLambda(_))
        afuns
      }
    }
  }

  def main(args: Array[String]) {



    parseArgs(args) match {
      case Help => {
        println(Config.helpMessage)
        System.exit(1)
      }
      case Compile(files, outpath) => {
        files.foreach(doCompile(_, outpath))
      }

      case Dependency(files, jarfile) => {
        val nuvoTypes = files.flatMap(extractDefinedTypes(_))
        val helperTypes = nuvoTypes map(_ + Config.helperSuffix)
        resolveClassDir map { path =>
          val type2class = (s: String) =>  s.replace('.', File.separatorChar) + Config.binExtension
          val binNuvoTypes = (nuvoTypes ::: (nuvoTypes map (_ + "$"))) map { type2class(_) }
          val binHelperTypes = (helperTypes ::: (helperTypes map (_ + "$"))) map { type2class(_) }
          val anonfuns = resolveAnonfunDependencies(nuvoTypes)

          val targets = binNuvoTypes ::: binHelperTypes ::: anonfuns
          val pathChange = Config.jarDirOpt + " " + path

          val cmdstr = Config.jarcmd + " " + Config.jaropt + " " + jarfile + " " + (" " /: targets)(_ +" " + s" $pathChange " + _)
          val p = Runtime.getRuntime.exec(cmdstr)
          val rv = p.waitFor()
          if (rv != 0) {
            println("Failed to create dependency jar")
          }
        }
      }
    }

  }
}
