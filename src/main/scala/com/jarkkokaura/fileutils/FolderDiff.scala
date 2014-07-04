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
    val DIR, FILE, SYM, OTHER = Value

    def apply(fileAttrs: BasicFileAttributes) = {
      if (fileAttrs.isRegularFile)
        FILE
      else if (fileAttrs.isDirectory)
        DIR
      else if (fileAttrs.isSymbolicLink)
        SYM
      else
        OTHER
    }
  }
  import NodeType._

  case class NodeTimestamps private (createTime: FileTime, modifyTime: FileTime)

  object NodeTimestamps {
    def apply(attributes: BasicFileAttributes): NodeTimestamps =
      NodeTimestamps(attributes.creationTime, attributes.lastModifiedTime)
  }

  case class NodeProperties private (size: Option[Long], timestamps: NodeTimestamps)

  object NodeProperties {
    def apply(attributes: BasicFileAttributes): NodeProperties = {
      val size = if (attributes.isRegularFile || attributes.isOther) Some(attributes.size) else None
      NodeProperties(size, NodeTimestamps(attributes))
    }
  }

  sealed trait FileTreeNode {
    val path: Path
    val nodeType: NodeType
    val properties: NodeProperties
  }

  case class FolderNode(path: Path, properties: NodeProperties, children: List[FileTreeNode]) extends FileTreeNode {
    val nodeType = DIR
  }

  case class FileNode(path: Path, nodeType: NodeType, properties: NodeProperties) extends FileTreeNode {
    require(nodeType == FILE || nodeType == SYM, "Incorrect node type for file: " + nodeType)
  }

  sealed trait PrintLine {
    val path: Path
    val side: Side

    def name = path.getFileName.toString
    def level = path.getNameCount
  }

  case class ParentFolderLine(path: Path, side: Side) extends PrintLine

  object DifferenceType extends Enumeration {
    type DifferenceType = Value
    val EXISTS_ONLY_ON_ONE_SIDE, NODE_TYPE, SIZE = Value
  }
  import DifferenceType._

  case class PathDifference private (
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
        case (Some(leftNode), Some(rightNode)) =>
          if (leftNode.nodeType != rightNode.nodeType)
            Some(PathDifference(NODE_TYPE, leftOpt, rightOpt))
          else if (leftNode.isInstanceOf[FileNode] && rightNode.isInstanceOf[FileNode] &&
            leftNode.asInstanceOf[FileNode].properties.size != rightNode.asInstanceOf[FileNode].properties.size)

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
        ret = FolderNode(folderPath.relativize(dir), NodeProperties(attrs), Nil) :: ret
        FileVisitResult.CONTINUE
      }
      override def visitFile(file: Path, attrs: BasicFileAttributes) = {
        val fileNode = FileNode(folderPath.relativize(file), NodeType(attrs), NodeProperties(attrs))
        ret = fileNode :: ret
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

  sealed trait Formatter[T, U <: StringWrapper] extends /*Function1[T, U] with*/ Width {
    lazy private[this] val empty = " " * width

    final def apply(opt: Option[T]): U = opt match {
      case Some(obj) => wrap(format(obj))
      case None => wrap(empty)
    }

    protected def format(t: T): String
    protected def wrap(s: String): U
  }

  case class NodeTypeW private (width: Int) extends WidthLike[NodeTypeW] with Formatter[NodeType, NodeTypeString] {
    override def max(that: NodeTypeW) = throw new UnsupportedOperationException

    lazy private[this] val formatter = rightPad(width)

    protected def format(nodeType: NodeType) = formatter format nodeType.toString
    protected def wrap(s: String) = NodeTypeString(s)
  }
  case class PathW(width: Int) extends WidthLike[PathW] with Formatter[Path, PathString] {
    lazy private[this] val formatter = rightPad(width)

    protected def format(path: Path) =
      formatter format (INDENT * (path.getNameCount - 1) + path.getFileName.toString)
    protected def wrap(s: String) = PathString(s)
  }
  case class SizeW(width: Int) extends WidthLike[SizeW] with Formatter[Long, SizeString] {
    lazy private[this] val formatter = "%" + width + "d"

    protected def format(size: Long) = formatter format size
    protected def wrap(s: String) = SizeString(s)
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

  type ColumnFormatInput = Either[Path, Option[Tuple2[DifferenceType, FileTreeNode]]]
  type ColumnFormatOutput = Tuple3[NodeTypeString, PathString, SizeString]

  case class ColumnFormatter private (typeFormatter: NodeTypeW, pathFormatter: PathW, sizeFormatter: SizeW) {
    def accomodate(tuple: (PathW, SizeW)) =
      ColumnFormatter(typeFormatter, pathFormatter.max(tuple._1), sizeFormatter.max(tuple._2))

    def apply(input: ColumnFormatInput): ColumnFormatOutput = input match {
      case Left(path) => (typeFormatter(Some(DIR)), pathFormatter(Some(path)), sizeFormatter(None))
      case Right(opt) => opt match {
        case Some((diffType, node)) =>
          (typeFormatter(Some(node.nodeType)), pathFormatter(Some(node.path)), sizeFormatter(node.properties.size))
        case None =>
          (typeFormatter(None), pathFormatter(None), sizeFormatter(None))
      }
    }
  }

  object ColumnFormatter {
    def apply(path: PathW, size: SizeW): ColumnFormatter = ColumnFormatter(NodeTypeW(), path, size)
  }

  type ResultLineFormatter = Tuple2[ColumnFormatter, ColumnFormatter]

  def getResultLineFormatter(diffs: Seq[PathDifference]): ResultLineFormatter = {
    val init = ColumnFormatter(0, 0)

    diffs.foldLeft(init -> init)((acc: ResultLineFormatter, diff: PathDifference) => {

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

      acc._1.accomodate(pathWidths._1 -> sizeWidths._1) -> acc._2.accomodate(pathWidths._2 -> sizeWidths._2)
    })
  }

  def getPrintLines(diffs: Seq[PathDifference]): Seq[PrintLine] = {
    val diffNodePathNames: Set[String] = diffs.map(_.path.toString)(breakOut)

    val ancestorNameLevelPairs: Set[(Path, Side)] =
      (for {
        diff <- diffs
        ancestorPath <- diff.ancestorPaths
        if !diffNodePathNames.contains(ancestorPath.toString)
      } yield (ancestorPath, diff.side))(breakOut)

    val parentFolderLines: Vector[ParentFolderLine] =
      ancestorNameLevelPairs.map(t => ParentFolderLine(t._1, t._2))(breakOut)

    (diffs ++ parentFolderLines).sortBy(_.path.toString)
  }

  case class PrintLineFormatResult(isDiffLine: Boolean, left: ColumnFormatOutput, right: ColumnFormatOutput)

  def formatLine(line: PrintLine, formatter: ResultLineFormatter): PrintLineFormatResult = {
    def columnFormatInput: Boolean => ColumnFormatInput = (selectLeft) => line match {
      case ParentFolderLine(path, _) => Left(path)
      case diff@PathDifference(diffType, _, _) => {
        val nodeOpt = if (selectLeft) diff.leftNode else diff.rightNode
        nodeOpt match {
          case Some(node) => Right(Some(diffType, node))
          case None => Right(None)
        }
      }
    }

    val isDiffLine = line.isInstanceOf[PathDifference]

    PrintLineFormatResult(isDiffLine, formatter._1(columnFormatInput(true)), formatter._2(columnFormatInput(false)))
  }

  def toString(output: ColumnFormatOutput): String = {
    val (nodeType, path, size) = output
    f"| $nodeType | $path | $size "
  }

  def formatOutput(resultLines: Seq[PrintLine], formatter: ResultLineFormatter): String = {
    resultLines map { formatLine(_, formatter) } map { (p: PrintLineFormatResult) =>
      val leftStr = toString(p.left)
      val rightStr = toString(p.right)
      val diffMarker = if (p.isDiffLine) "\u2192" else " "

      f"$diffMarker $leftStr$rightStr|"
    } mkString "\n"
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

  println(formatOutput(getPrintLines(diffs), getResultLineFormatter(diffs)))

}
