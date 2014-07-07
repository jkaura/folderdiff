package com.jarkkokaura.fileutils

import java.io.File
import java.nio.file.FileSystems
import java.nio.file.FileVisitResult
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.SimpleFileVisitor
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.attribute.FileTime
import java.util.Iterator

import scala.collection.breakOut
import scala.collection.JavaConversions.asScalaIterator
import scala.language.implicitConversions

object FolderDiff extends App {

  object Utils {
    def map[T, U](tuple: Tuple2[Option[T], Option[T]], fn: T => U): Tuple2[Option[U], Option[U]] =
      tuple._1.map(fn) -> tuple._2.map(fn)
    def rightPad(n: Int) = "%1$-" + n + "s"
  }
  import Utils._

  object Side extends Enumeration {
    type Side = Value
    val LEFT, RIGHT, BOTH = Value

    def apply[T](tuple: Tuple2[Option[T], Option[T]]): Side = tuple match {
      case (Some(_), None) => LEFT
      case (None, Some(_)) => RIGHT
      case (Some(_), Some(_)) => BOTH
      case _ => throw new IllegalArgumentException
    }
  }
  import Side._

  object NodeType extends Enumeration {
    type NodeType = Value
    val DIRECTORY = Value("DIR")
    val FILE = Value
    val SYMBOLIC_LINK = Value("SYM")
    val OTHER = Value("???")

    def apply(fileAttrs: BasicFileAttributes) = {
      if (fileAttrs.isRegularFile)
        FILE
      else if (fileAttrs.isDirectory)
        DIRECTORY
      else if (fileAttrs.isSymbolicLink)
        SYMBOLIC_LINK
      else
        OTHER
    }
  }
  import NodeType._

  case class NodeTimestamps(createTime: FileTime, modifyTime: FileTime)

  object NodeTimestamps {
    def apply(attributes: BasicFileAttributes): NodeTimestamps =
      NodeTimestamps(attributes.creationTime, attributes.lastModifiedTime)
  }

  case class NodeProperties(size: Option[Long], timestamps: NodeTimestamps)

  object NodeProperties {
    def apply(attributes: BasicFileAttributes): NodeProperties = {
      val sizeOpt = if (attributes.isRegularFile || attributes.isOther) Some(attributes.size) else None
      NodeProperties(sizeOpt, NodeTimestamps(attributes))
    }
  }

  case class FileTreeNode(path: Path, nodeType: NodeType, properties: NodeProperties) {
    def hasSize = properties.size.isDefined
  }

  object DifferenceType extends Enumeration {
    type DifferenceType = Value
    val EXISTS_ONLY_ON_ONE_SIDE = Value("MISS")
    val NODE_TYPE = Value("TYPE")
    val SIZE = Value
  }
  import DifferenceType._

  sealed trait PrintLine { val path: Path }

  case class ParentFolderLine(path: Path) extends PrintLine

  case class PathDifference(
    diffType: DifferenceType, leftNode: Option[FileTreeNode], rightNode: Option[FileTreeNode])
    extends Ordered[PathDifference] with PrintLine {

    require(leftNode.isDefined || rightNode.isDefined, "At least one node must be non-empty")

    lazy val side = Side(asNodeTuple)
    lazy val path = leftNode.orElse(rightNode).get.path

    private[FolderDiff] def asNodeTuple = leftNode -> rightNode
    def compare(that: PathDifference) = path.compareTo(that.path)
    def ancestorPaths: Seq[Path] = 1 until path.getNameCount map { path.subpath(0, _) }
  }

  object PathDifference {
    def apply(leftOpt: Option[FileTreeNode], rightOpt: Option[FileTreeNode]): Option[PathDifference] =
      (leftOpt, rightOpt) match {
        case (None, None) => None
        case (Some(_), None) => Some(PathDifference(EXISTS_ONLY_ON_ONE_SIDE, leftOpt, rightOpt))
        case (None, Some(_)) => Some(PathDifference(EXISTS_ONLY_ON_ONE_SIDE, leftOpt, rightOpt))
        case (
          Some(FileTreeNode(_, leftNodeType, NodeProperties(leftSizeOpt, _))),
          Some(FileTreeNode(_, rightNodeType, NodeProperties(rightSizeOpt, _)))
          ) =>

          if (leftNodeType != rightNodeType)
            Some(PathDifference(NODE_TYPE, leftOpt, rightOpt))
          else if (leftSizeOpt exists { ls => rightSizeOpt exists { rs => ls != rs } })
            Some(PathDifference(SIZE, leftOpt, rightOpt))
          else
            None
      }
  }

  def collectFileTreeNodes(folder: File): List[FileTreeNode] = {
    require(folder.isDirectory(), "Argument is not a directory: " + folder)

    val folderPath = FileSystems.getDefault.getPath(folder.getAbsolutePath);
    var ret: List[FileTreeNode] = Nil

    val visitor = new SimpleFileVisitor[Path] {
      override def preVisitDirectory(dir: Path, attrs: BasicFileAttributes) = {
        ret = FileTreeNode(folderPath.relativize(dir), DIRECTORY, NodeProperties(attrs)) :: ret
        FileVisitResult.CONTINUE
      }
      override def visitFile(file: Path, attrs: BasicFileAttributes) = {
        ret = FileTreeNode(folderPath.relativize(file), NodeType(attrs), NodeProperties(attrs)) :: ret
        FileVisitResult.CONTINUE
      }
    }

    Files.walkFileTree(folderPath, visitor)
    ret
  }

  def differences(leftTree: Seq[FileTreeNode], rightTree: Seq[FileTreeNode]): Seq[PathDifference] = {
    val indexFn = (n: FileTreeNode) => n.path.toString -> n
    val leftMap: Map[String, FileTreeNode] = leftTree.map(indexFn)(breakOut)
    val rightMap: Map[String, FileTreeNode] = rightTree.map(indexFn)(breakOut)

    val leftDiffs: Seq[PathDifference] =
      (for {
        (leftPath, leftNode) <- leftMap
        diff <- PathDifference(Some(leftNode), rightMap.get(leftPath))
      } yield diff)(breakOut)

    val rightOnlyDiffs: Seq[PathDifference] =
      (for {
        (_, rightNode) <- rightMap -- leftMap.keys
        diff <- PathDifference(None, Some(rightNode))
      } yield diff)(breakOut)

    (leftDiffs ++ rightOnlyDiffs).sorted
  }

  val INDENT = "  "

  sealed trait StringWrapper {
    val result: String
    override def toString = result
  }
  case class DiffTypeString(result: String) extends StringWrapper
  case class NodeTypeString(result: String) extends StringWrapper
  case class PathString(result: String) extends StringWrapper
  case class SizeString(result: String) extends StringWrapper

  sealed trait Width {
    val width: Int
  }
  sealed trait WidthLike[T <: Width] extends Ordered[T] {
    self: T =>
    def compare(that: T) = width compareTo that.width
    def max(that: T) = if (this > that) this else that
  }

  sealed trait Formatter[T, U <: StringWrapper] extends Width {
    lazy private[this] val empty = " " * width
    lazy private[this] val leftPaddingFormatter = "%" + width + "s"
    lazy private[this] val rightPaddingFormatter = rightPad(width)

    val rightPadding = true

    final def apply(opt: Option[T]): U = opt match {
      case Some(obj) => {
        val padFormatter = if (rightPadding) rightPaddingFormatter else leftPaddingFormatter
        wrap(padFormatter format format(obj))
      }
      case None => wrap(empty)
    }

    protected def format(t: T): String
    protected def wrap(s: String): U
  }

  case class DiffTypeW(width: Int) extends WidthLike[DiffTypeW] with Formatter[DifferenceType, DiffTypeString] {
    override def max(that: DiffTypeW) = throw new UnsupportedOperationException
    protected def format(diffType: DifferenceType) = diffType.toString
    protected def wrap(s: String) = DiffTypeString(s)
  }
  case class NodeTypeW(width: Int) extends WidthLike[NodeTypeW] with Formatter[NodeType, NodeTypeString] {
    override def max(that: NodeTypeW) = throw new UnsupportedOperationException
    protected def format(nodeType: NodeType) = nodeType.toString
    protected def wrap(s: String) = NodeTypeString(s)
  }
  case class PathW(width: Int) extends WidthLike[PathW] with Formatter[Path, PathString] {
    lazy private[this] val formatter = rightPad(width)
    protected def format(path: Path) = INDENT * (path.getNameCount - 1) + path.getFileName.toString
    protected def wrap(s: String) = PathString(s)
  }
  case class SizeW(width: Int) extends WidthLike[SizeW] with Formatter[Long, SizeString] {
    override val rightPadding = false
    protected def format(size: Long) = size.toString
    protected def wrap(s: String) = SizeString(s)
  }

  object DiffTypeW {
    private[this] val WIDTH = DifferenceType.values map { _.toString.length } reduce { _ max _ }
    def apply(): DiffTypeW = DiffTypeW(WIDTH)
  }

  object NodeTypeW {
    private[this] val WIDTH = NodeType.values map { _.toString.length } reduce { _ max _ }
    def apply(): NodeTypeW = NodeTypeW(WIDTH)
  }

  object PathW {
    def apply(path: Path): PathW = {
      val iter = path.iterator
      val indentLen = INDENT.length
      var elemIdx = 0
      var maxElemWidth = 0

      while (iter.hasNext) {
        val path = iter.next
        val fileNameLen = path.getFileName.toString.length
        val width = elemIdx * indentLen + fileNameLen
        maxElemWidth = maxElemWidth.max(width)
        elemIdx += 1
      }

      PathW(maxElemWidth)
    }

    implicit def fromInt(i: Int) = PathW(i)
  }

  object SizeW {
    def apply(size: Long): SizeW = SizeW(Math.log10(size).toInt + 1)
    implicit def fromLong(l: Long) = apply(l)
  }

  type ColumnFormatterInput = Either[Path, Option[FileTreeNode]]

  case class ColumnFormatterOutput(nodeType: NodeTypeString, path: PathString, size: SizeString) {
    override def toString = f"| $nodeType | $path | $size "
  }

  case class PrintLineColumnFormatter(nodeTypeFormatter: NodeTypeW, pathFormatter: PathW, sizeFormatter: SizeW) {
    def accommodate(tuple: (PathW, SizeW)) =
      PrintLineColumnFormatter(nodeTypeFormatter, pathFormatter.max(tuple._1), sizeFormatter.max(tuple._2))

    def apply(input: ColumnFormatterInput): ColumnFormatterOutput = {
      val (nodeTypeOpt, pathOpt, sizeOpt) = input match {
        case Left(path) => (Some(DIRECTORY), Some(path), None)
        case Right(opt) => opt match {
          case Some(FileTreeNode(path, nodeType, NodeProperties(size, _))) => (Some(nodeType), Some(path), size)
          case None => (None, None, None)
        }
      }

      ColumnFormatterOutput(nodeTypeFormatter(nodeTypeOpt), pathFormatter(pathOpt), sizeFormatter(sizeOpt))
    }
  }

  object PrintLineColumnFormatter {
    def apply(path: PathW, size: SizeW): PrintLineColumnFormatter = PrintLineColumnFormatter(NodeTypeW(), path, size)
  }

  type PrintLineFormatterInput = Tuple3[Option[DifferenceType], ColumnFormatterInput, ColumnFormatterInput]

  case class PrintLineFormatterOutput(
    diffType: DiffTypeString, left: ColumnFormatterOutput, right: ColumnFormatterOutput)

  case class PrintLineFormatter(
    diffType: DiffTypeW, leftCol: PrintLineColumnFormatter, rightCol: PrintLineColumnFormatter) {

    def apply(input: PrintLineFormatterInput) =
      PrintLineFormatterOutput(diffType(input._1), leftCol(input._2), rightCol(input._2))

    def accommodate(leftTuple: (PathW, SizeW), rightTuple: (PathW, SizeW)) =
      PrintLineFormatter(diffType, leftCol.accommodate(leftTuple), rightCol.accommodate(rightTuple))
  }

  object PrintLineFormatter {
    def apply(leftCol: PrintLineColumnFormatter, rightCol: PrintLineColumnFormatter): PrintLineFormatter =
      PrintLineFormatter(DiffTypeW(), leftCol, rightCol)
  }

  def getPrintLineFormatter(diffs: Seq[PathDifference]): PrintLineFormatter = {
    val initCol = PrintLineColumnFormatter(0, 0)

    diffs.foldLeft(PrintLineFormatter(initCol, initCol))((acc: PrintLineFormatter, diff: PathDifference) => {

      val pathWidth = PathW(diff.path)

      val pathWidths: (PathW, PathW) = diff.side match {
        case LEFT => (pathWidth, 0)
        case RIGHT => (0, pathWidth)
        case BOTH => (pathWidth, pathWidth)
      }

      val getSize = (_: Option[FileTreeNode]) match {
        case Some(node) => node.properties.size.getOrElse(0L)
        case _ => 0L
      }

      val sizeWidths = SizeW(getSize(diff.leftNode)) -> SizeW(getSize(diff.rightNode))

      acc.accommodate(pathWidths._1 -> sizeWidths._1, pathWidths._2 -> sizeWidths._2)
    })
  }

  def getPrintLines(diffs: Seq[PathDifference]): Seq[PrintLine] = {
    val diffNodePathNames: Set[String] = diffs.map(_.path.toString)(breakOut)

    val parentFolderLines: Set[ParentFolderLine] =
      (for {
        diff <- diffs
        ancestorPath <- diff.ancestorPaths
        if !diffNodePathNames.contains(ancestorPath.toString)
      } yield ParentFolderLine(ancestorPath))(breakOut)

    (diffs ++ parentFolderLines).sortBy(_.path.toString)
  }

  def getPrintLineFormatterInput(line: PrintLine): PrintLineFormatterInput = {
    def columnFormatInput: Boolean => ColumnFormatterInput = (selectLeft) => line match {
      case ParentFolderLine(path) => Left(path)
      case PathDifference(_, leftNodeOpt, rightNodeOpt) => Right(if (selectLeft) leftNodeOpt else rightNodeOpt)
    }

    val diffTypeOpt = line match {
      case ParentFolderLine(_) => None
      case PathDifference(diffType, _, _) => Some(diffType)
    }

    (diffTypeOpt, columnFormatInput(true), columnFormatInput(false))
  }

  def getOutputLines(resultLines: Seq[PrintLine], formatter: PrintLineFormatter): Seq[String] =
    resultLines map { l => formatter(getPrintLineFormatterInput(l)) } map {
      (p: PrintLineFormatterOutput) => f"${p.diffType} ${p.left}${p.right}|"
    }

  def printUsageAndExit: Unit = {
    println("Usage: FolderDiff <first_folder> <second_folder>")
    System.exit(0)
  }

  // Main body

  val (leftFolder: File, rightFolder: File) = args match {
    case Array(left, right) => new File(left) -> new File(right)
    case _ => printUsageAndExit
  }

  if (!(leftFolder.isDirectory && rightFolder.isDirectory)) printUsageAndExit

  val diffs = differences(collectFileTreeNodes(leftFolder), collectFileTreeNodes(rightFolder))

  getOutputLines(getPrintLines(diffs), getPrintLineFormatter(diffs)) foreach { println }

  println("Found " + diffs.size + " differences.")

}
