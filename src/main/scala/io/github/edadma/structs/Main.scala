package io.github.edadma.structs

import io.github.edadma.json
import io.github.edadma.mustache._

import scopt.OParser

import java.io.File
import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object Main extends App {
  case class Config(file: File, start: Int, end: Option[Int])

  val builder = OParser.builder[Config]
  val parser = {
    import builder._

    OParser.sequence(
      programName("externs"),
      head("Bindings Extern Generator", "v0.1.0"),
      opt[Option[Int]]('e', "end")
        .optional()
        .action((e, c) => c.copy(end = e))
        .text("start line number (optional)"),
      help('h', "help").text("prints this usage text"),
      opt[Int]('s', "start")
        .optional()
        .action((s, c) => c.copy(start = s))
        .text("start line number (optional)"),
      version('v', "version").text("prints the version"),
      arg[File]("<file>")
        .required()
        .action((f, c) => c.copy(file = f))
        .validate(f =>
          if (f.exists && f.isFile && f.canRead) success
          else failure("<file> must exist and be a readable file"))
        .text("path to text file to open")
    )
  }

  val const = "/*const*/ "

  val type2string: PartialFunction[TypeAST, String] = {
    case PointerType(PrimitiveType("CChar", const), _) => s"${if (const) const else ""}CString"
    case PointerType(typ, const)                       => s"${if (const) const else ""}Ptr[${type2string(typ)}]"
    case PrimitiveType(name, const)                    => s"${if (const) const else ""}$name"
    case TypedefType(Ident(_, name), const)            => s"${if (const) const else ""}$name"
  }

  OParser.parse(parser, args, Config(null, 1, None)) match {
    case Some(conf) => app(conf)
    case _          =>
  }

  def app(conf: Config): Unit = {
    val lines                          = util.Using(scala.io.Source.fromFile(conf.file.getPath))(_.getLines() to ArraySeq).get
    val section                        = lines dropRight (lines.length - conf.end.getOrElse(0)) drop (conf.start - 1)
    val StructDeclarationsAST(externs) = StructsParser.parseHeader(section mkString "\n")
    val list                           = new ListBuffer[json.Object]

    for (StructDeclarationAST(name, members) <- externs) {
      list += json.Object("name"    -> name.s,
                          "members" -> membersData(members),
                          "count"   -> members.length,
                          "line"    -> (conf.start + name.pos.line - 1).toString)
    }

    val data = json.Object("structs" -> json.Array(list))

    val template =
      """
       |{{#structs}}
       |type {{name}} = CStruct{{count}}[{{#members}}{{type}}{{comma}}{{/members}}] //{{line}}
       |
       |implicit class {{name}}(val ptr: Ptr[lib.{{name}}]) extends AnyVal {
       |{{#members}}
       |  def {{name}}: {{type}} = ptr._{{ordinal}}
       |{{/members}}
       |
       |{{#members}}
       |  def {{name}}_=(v: {{type}}) = ptr._{{ordinal}} = v
       |{{/members}}
       |}
       |{{/structs}}
       |""".trim.stripMargin

    println(processMustache(data, template, "trim" -> false, "removeNonSectionBlanks" -> false))
  }

  def camel(s: String): String = {
    val segs = s.split("_")
    val buf  = new StringBuilder

    buf ++= segs(0)

    for (i <- segs.indices drop 1)
      buf ++= segs(i).head.toUpper +: segs(i).tail

    buf.toString
  }

  def membersData(ms: List[MemberAST]): json.Array = {
    val array =
      ms flatMap {
        case MemberAST(ns, typ) => ns map { case Ident(_, name) => (camel(name), type2string(typ)) }
      }

    json.Array(array.zipWithIndex map {
      case ((n, t), i) =>
        json.Object("name"    -> n,
                    "type"    -> t,
                    "ordinal" -> (i + 1),
                    "comma"   -> (if (i == array.length - 1) "" else ", "))
    })
  }

}
