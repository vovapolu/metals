package scala.meta.internal.metals

import java.util
import java.util.concurrent.ConcurrentHashMap
import java.nio.file._
import java.nio.file.attribute.BasicFileAttributes

import scala.concurrent.ExecutionContext
import scala.meta._
import org.eclipse.lsp4j.{
  PublishDiagnosticsParams,
  DiagnosticSeverity,
  Diagnostic,
  Range,
  Position
}

import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.mtags.Semanticdbs

import zio.shield.ConfiguredZioShield
import zio.shield.SemanticDocumentLoader
import zio.shield.{ZioShield, ZioShieldDiagnostic}
import zio.shield.config
import scalafix.shield.ZioShieldScalafixExtension
import scalafix.v1.{SemanticDocument, SyntacticDocument}

final class ZioShieldProvider(
    workspace: AbsolutePath,
    workspaceFolders: List[AbsolutePath],
    buildTargets: BuildTargets,
    client: MetalsLanguageClient,
    semanticDbs: Semanticdbs,
    userConfiguration: () => UserConfiguration
)(implicit ec: ExecutionContext) {

  var zioShield: ConfiguredZioShield = _

  // metals calls onChange too often
  // updating ZIO Shield cache is a heavy operation, so we don't want to call it so often
  private val lastCached: ConcurrentHashMap[Path, Long] =
    new ConcurrentHashMap()
  private val lastDiagnostics: ConcurrentHashMap[Path, Long] =
    new ConcurrentHashMap()

  private def filterNewFiles(
      timeMap: ConcurrentHashMap[Path, Long],
      files: List[Path]
  ): List[Path] = files.filter { f =>
    if (timeMap.containsKey(f)) {
      timeMap.get(f) + ZioShieldProvider.lastChangedTimeout <
        System.currentTimeMillis()
    } else {
      true
    }
  }

  private def markFiles(
      timeMap: ConcurrentHashMap[Path, Long],
      files: List[Path]
  ): Unit = {
    val curTime = System.currentTimeMillis()
    files.foreach { f =>
      timeMap.put(f, curTime)
    }
  }

  private def zioShieldConfigPath: AbsolutePath = {
    val configpath = userConfiguration().zioShieldConfigPath
    (workspace :: workspaceFolders).iterator
      .map(_.resolve(configpath))
      .collectFirst { case path if path.isFile => path }
      .getOrElse(workspace.resolve(configpath))
  }

  def initZioShield() = {
    val fullClasspath =
      PackageIndex.bootClasspath.map(_.toNIO) ++ PackageIndex.scalaLibrary ++
        buildTargets.all.flatMap(_.scalac.classpath.map(_.toNIO))

    val zioShieldExtension = new ZioShieldScalafixExtension(fullClasspath)

    val semanticDocumentLoader = new SemanticDocumentLoader {
      def load(
          synDoc: SyntacticDocument,
          path: Path
      ): Either[Throwable, SemanticDocument] = {
        semanticDbs
          .textDocument(AbsolutePath(path))
          .getE
          .map(td => zioShieldExtension.semanticDocumentFromTextDocument(synDoc, td))
      }
    }

    val zioShieldConfig = config.Config.fromFile(zioShieldConfigPath.toNIO)

    zioShieldConfig match {
      case Left(err) => 
        scribe.error(s"Unable to load ZIO Shield config: ${err}\nUsing all possible rules.")
        zioShield = ZioShield(semanticDocumentLoader).withAllRules()
      case Right(conf) => 
        zioShield = ZioShield(semanticDocumentLoader).withConfig(conf)
    }
  }

  def buildCache(dirs: Iterable[Path]): Unit = {
    initZioShield()

    val sourceFiles = new util.ArrayList[Path]()

    dirs.toList.foreach { dir =>
      if (Files.exists(dir)) {
        Files.walkFileTree(
          dir,
          new SimpleFileVisitor[Path] {
            override def visitFile(
                file: Path,
                attrs: BasicFileAttributes
            ): FileVisitResult = {
              val filename = file.getFileName.toString
              if (filename.endsWith(".scala") || filename.endsWith(".java")) {
                sourceFiles.add(file)
              }
              FileVisitResult.CONTINUE
            }
          }
        )
      }
    }

    zioShield.updateCache(sourceFiles.asScala.toList)(
      d => scribe.error(s"ZIO Shield: ${d.consoleMessage}")
    )
  }

  def updateCache(files: List[Path]): Unit = this.synchronized {    
    val newFiles = filterNewFiles(lastCached, files)
    markFiles(lastCached, newFiles)

    if (newFiles.nonEmpty) {
      scribe.info(s"Updating ZIO Shield cache on ${newFiles.mkString(",")}")

      zioShield.updateCache(newFiles)(
        d => scribe.info(s"ZIO Shield: ${d.consoleMessage}")
      )     
    }
  }

  def runShield(files: List[Path]): Unit = this.synchronized {
    if (zioShield != null) {
      val newFiles = filterNewFiles(lastDiagnostics, files)
      markFiles(lastDiagnostics, files)
      if (newFiles.nonEmpty) {
        scribe.info(s"Running ZIO Shield on ${newFiles.mkString(", ")}")

        val diagnostics = new util.ArrayList[ZioShieldDiagnostic]()
        zioShield.run(newFiles)(d => diagnostics.add(d))

        newFiles.foreach { file =>
          val fileDiagnostics = new util.ArrayList[Diagnostic]()

          diagnostics.forEach {
            case ZioShieldDiagnostic.Lint(dFile, pos, msg) if dFile == file =>
              fileDiagnostics.add(
                new Diagnostic(
                  new Range(
                    new Position(pos.startLine, pos.startColumn),
                    new Position(pos.endLine, pos.endColumn)
                  ),
                  msg,
                  DiagnosticSeverity.Warning,
                  "ZIO Shield"
                )
              )
            case _ =>
          }

          client.publishDiagnostics(
            new PublishDiagnosticsParams(
              file.toUri.toString,
              fileDiagnostics
            )
          )
        }
      }
    } else {
      scribe.error("ZIO Shield hasn't been initialized")
    }
  }
}

object ZioShieldProvider {
  val lastChangedTimeout = 1000L
}
