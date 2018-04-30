package org.snrostov.kl.entities

annotation class Ast(val name: String)

open class KlMember

class KlFunction(
  val name: String,
  val typeParameters: List<KlTypeParameter>,
  val extensionReceiver: KlType,
  val valueParameters: List<KlValueParameter>,
  val explicitReturnType: KlType,
  val statements: List<KlStatement>
)

class KlValueParameter(
  val name: String,
  val type: KlType,
  val default: KlExpr
)

class KlClass(
  val name: String,
  val typeParameters: List<KlTypeParameter>,
  val superTypes: List<SuperTypeEntry>,
  val members: List<KlMember>
) : KlMember() {
  class SuperTypeEntry(val type: KlType, val delegate: KlExpr)
}

class KlTypeParameter(
  val name: String,
  val lowerBound: KlType
)

class KlType(
  val typeConstructor: KlClass,
  val typeArguments: List<KlTypeProjection>
)

class KlTypeProjection(
  val projection: String,
  val type: KlType
)

abstract class KlExpr

sealed class KlStatement