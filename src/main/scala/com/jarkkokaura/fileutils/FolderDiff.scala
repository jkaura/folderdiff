package com.jarkkokaura.fileutils

import java.io.File
import java.nio.file.FileSystems
import java.nio.file.FileVisitResult
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.SimpleFileVisitor
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.attribute.FileAttribute
import java.nio.file.attribute.FileTime
import scala.collection.breakOut

object FolderDiff extends App {

  object Side extends Enumeration {
    type Side = Value
    val LEFT, RIGHT = Value
  }
  import Side._

  object NodeType extends Enumeration {
    type NodeType = Value
    val DIRECTORY, FILE, SYMBOLIC_LINK, OTHER = Value

    def apply(fileAttrs: BasicFileAttributes) = {
      if (fileAttrs.isRegularFile) {
        FILE
      } else if (fileAttrs.isDirectory()) {
        DIRECTORY
      } else if (fileAttrs.isSymbolicLink()) {
        SYMBOLIC_LINK
      } else {
        OTHER
      }
    }
  }
  import NodeType._

  case class NodeTimestamps(createTime: FileTime, modifyTime: FileTime) {
    def this(fileAttributes: BasicFileAttributes) = this(fileAttributes.creationTime, fileAttributes.lastModifiedTime)
  }

  sealed trait NodeProperties {
    val timestamps: NodeTimestamps
  }
  case class FolderProperties(timestamps: NodeTimestamps) extends NodeProperties {
    def this(fileAttributes: BasicFileAttributes) = this(new NodeTimestamps(fileAttributes))
  }
  case class FileProperties(size: Long, timestamps: NodeTimestamps) extends NodeProperties {
    def this(fileAttributes: BasicFileAttributes) = this(fileAttributes.size, new NodeTimestamps(fileAttributes))
  }

  sealed trait FileTreeNode {
    type T <: NodeProperties

    val path: Path
    val nodeType: NodeType
    val properties: T

    def propertiesDifference(that: FileTreeNode): Option[PathDifference] = None

    final def difference(that: FileTreeNode): Option[PathDifference] =
      if (nodeType != that.nodeType)
        Some(TypeDifference(path, nodeType, that.nodeType))
      else
        propertiesDifference(that)
  }

  case class FolderNode(path: Path, properties: FolderProperties, children: List[FileTreeNode]) extends FileTreeNode {
    type T = FolderProperties
    lazy val nodeType = DIRECTORY
  }

  case class FileNode(path: Path, nodeType: NodeType, properties: FileProperties) extends FileTreeNode {

    require(nodeType == FILE || nodeType == SYMBOLIC_LINK, "Incorrect node type for file: " + nodeType)

    type T = FileProperties

    override def propertiesDifference(that: FileTreeNode) =
      that.properties match {
        case thatProps@FileProperties(thatSize, _) if properties.size != thatSize =>
          Some(SizeDifference(path, properties, thatProps))
        case _ => None
      }
  }

  sealed trait PathDifference {
    val path: Path
  }
  case class ExistsOnlyOnOneSide(side: Side, node: FileTreeNode) extends PathDifference {
    lazy val path = node.path
  }
  case class TypeDifference(path: Path, leftType: NodeType, rightType: NodeType) extends PathDifference {
    require(leftType != rightType, "Node types must differ")
  }
  case class SizeDifference(path: Path, leftProperties: FileProperties, rightProperties: FileProperties)
    extends PathDifference {

    require(leftProperties.size != rightProperties.size, "Sizes must differ")
  }

  implicit def pathDifferenceOrdering[T <: PathDifference] =
    new Ordering[T] {
      def compare(x: T, y: T) = x.path.compareTo(y.path);
    }

  def collectFileTreeNodes(folder: File): List[FileTreeNode] = {
    require(folder.isDirectory(), "Argument is not a directory: " + folder)

    val folderPath = FileSystems.getDefault().getPath(folder.getAbsolutePath());
    var ret: List[FileTreeNode] = Nil

    val visitor = new SimpleFileVisitor[Path] {
      override def preVisitDirectory(dir: Path, attrs: BasicFileAttributes) = {
        ret = FolderNode(folderPath.relativize(dir), new FolderProperties(attrs), Nil) :: ret
        FileVisitResult.CONTINUE
      }
      override def visitFile(file: Path, attrs: BasicFileAttributes) = {
        val fileNode = FileNode(folderPath.relativize(file), NodeType(attrs), new FileProperties(attrs))
        ret = fileNode :: ret
        FileVisitResult.CONTINUE
      }
    }

    Files.walkFileTree(folderPath, visitor)
    ret
  }

  def differences(leftTree: Seq[FileTreeNode], rightTree: Seq[FileTreeNode]): Seq[PathDifference] = {
    val indexFn = (n: FileTreeNode) => (n.path.toString, n)
    val leftMap: Map[String, FileTreeNode] = leftTree.map(indexFn)(breakOut)
    val rightMap: Map[String, FileTreeNode] = rightTree.map(indexFn)(breakOut)

    var commonPaths: Set[String] = Set()
    var uniqueLeftNodes: IndexedSeq[FileTreeNode] = Vector()

    leftMap foreach { pair =>
      if (rightMap.contains(pair._1))
        commonPaths += pair._1
      else
        uniqueLeftNodes = uniqueLeftNodes :+ pair._2
    }

    val uniqueRightNodes: IndexedSeq[FileTreeNode] =
      (for {
        (path, node) <- rightMap
        if !commonPaths.contains(path)
      } yield node)(breakOut)

    val leftOnlyDiffs = uniqueLeftNodes map (ExistsOnlyOnOneSide(LEFT, _))
    val rightOnlyDiffs = uniqueRightNodes map (ExistsOnlyOnOneSide(RIGHT, _))

    val differingNodes: IndexedSeq[PathDifference] =
      (for {
        path <- commonPaths
        diff <- leftMap(path).difference(rightMap(path))
      } yield diff)(breakOut)

    (leftOnlyDiffs ++ rightOnlyDiffs ++ differingNodes).sorted
  }

  def printUsageAndExit: Unit = {
    println("FolderDiff <first_folder> <second_folder>")
    System.exit(1);
  }

  // Main body

  if (args == null || args.size != 2) {
    printUsageAndExit
  }

  val leftFolder = new File(args(0))
  val rightFolder = new File(args(1))

  if (!(leftFolder.isDirectory() && rightFolder.isDirectory())) {
    printUsageAndExit
  }

  val leftFileTree = collectFileTreeNodes(leftFolder)
  val rightFileTree = collectFileTreeNodes(rightFolder)

  val diffs = differences(leftFileTree, rightFileTree)

  diffs.foreach {
    println(_)
  }

}