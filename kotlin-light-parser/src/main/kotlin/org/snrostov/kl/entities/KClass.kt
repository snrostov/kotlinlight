package org.snrostov.kl.entities

annotation class Ast(val name: String)

class KlMember

class KlClass(
  val name: String,
  val typeParameters: List<KlTypeParameter>,
  val superTypes: List<SuperTypeEntry>
) {
  class SuperTypeEntry(val type: KlTypeRef, val delegate: KlExpr)
}

class KlTypeParameter(
  val name: String,
  val lowerBound: KlTypeRef
)

class KlTypeRef(
  val typeConstructor: KlClass,
  val typeArguments: List<KlTypeProjection>
)

class KlTypeProjection(
  val projection: String,
  val type: KlTypeRef
)

abstract class KlExpr