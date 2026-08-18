package regression

import daisy._
import daisy.frontend._
import org.scalatest.funsuite.AnyFunSuite
import java.io.File

class TreesitterParsingTest extends AnyFunSuite {

  private val knownUnsupported: Set[String] = Set("fptaylor-extra.fpcore", "apron.fpcore", "precimonious.fpcore", "rosa.fpcore", "salsa.fpcore")

  /** Returns the list of source files in a directory with a given extension, sorted by name. */ 
  private def sourcesIn(dir: String, extension: String): List[File] =
    Option(new File(dir).listFiles).toList.flatten
      .filter(f => f.isFile && f.getName.endsWith(extension))
      .sortBy(_.getName)

  private def register(dir: String, extension: String, label: String)
                      (make: Context => TreesitterCommon): Unit = {
    val files = sourcesIn(dir, extension)
    if (files.isEmpty) {
      ignore(s"$label: no sources in $dir") {}
    }
    for (f <- files) {
      val name = f.getName
      if (knownUnsupported(name)) {
        ignore(s"$label: $name (known unsupported)") {}
      } else {
        test(s"$label: $name") {
          val ctx = Main.processOptions(List(f.getPath, "--silent")).get
          Main.ctx = ctx
          make(ctx).run
        }
      }
    }
  }

  register("testcases/fpbench-c-individual", ".c", "C")(new TreesitterC(_))
  register("testcases/fpbench", ".scala", "Scala")(new TreesitterScala(_))
  register("testcases/fpbench-fpcore", ".fpcore", "FPCore")(new TreesitterFPCore(_))
}
