package io.github.edadma.structs

import scala.util.parsing.input.Position

case class Ident(pos: Position, s: String)

trait AST
case class StructDeclarationsAST(structs: List[StructDeclarationAST])  extends AST
case class StructDeclarationAST(name: Ident, members: List[MemberAST]) extends AST
case class MemberAST(name: List[Ident], typ: TypeAST)                  extends AST

trait TypeAST                                          extends AST { val const: Boolean }
case class PrimitiveType(name: String, const: Boolean) extends TypeAST
case class TypedefType(name: Ident, const: Boolean)    extends TypeAST
case class PointerType(typ: TypeAST, const: Boolean)   extends TypeAST
