package co.actioniq.functional

import scala.language.experimental.macros
import scala.reflect.macros.Context

/**
 * Creates an instance of a trait using scala macros.
 *
 * Instance is of a class that extends the transitive closure of all self-types.
 *
 * This was written to encourage the usage of self-types, which nicely hides
 * details of those using a trait, but doesn't require then later instantiating
 * a class with all those types mixed in.  The InstanceFactory does the
 * mixing automatically.
 *
 * For example:
 *
 * trait Simple       { val a = "simple" }
 * trait SelfType     { self: Simple => val b = "selftype" }
 * trait NextSelfType { self: SelfType => val c = "nextselftype" }
 * InstanceFactory.newInstance[NextSelfType]()
 *
 * This generates code that essentially does:
 * {
 *   class Class1234 extends NextSelfType with SelfType with Simple
 *   new Class1234
 * }
 */
object InstanceFactory {

  /**
   * Creates an instance of a trait T using scala macro.
   *
   * The instance is of a class that extends the transitive closure of all self-types
   * annotations of the given trait T.
   *
   * As well as mixes in the additionalTypes, and their self-type annotations.
   *
   * (Be careful using the additional types. They must be resolvable at compile time.)
   */
  def newInstance[T](additionalTypes: Class[_]*): T = macro newInstanceMacro[T]

  // (macros must be public and long)
  // scalastyle:off
  def newInstanceMacro[T: c.WeakTypeTag](c: Context)(additionalTypes: c.Expr[Class[_]]*): c.Expr[T] = {
    import c.universe._

    /** Recursively get all self-types */
    def getSelfTypes(symbol: c.universe.ClassSymbol): List[c.universe.ClassSymbol] = {

      // TODO: This won't work if there are loops in the self-types.
      symbol :: {
        symbol.selfType match {
          // self-type is sometimes a refined type of a refined type
          case RefinedType(_ :: RefinedType(refinedTypes, _) :: Nil, _) =>
            // TODO: This foreach fixes some strange bug in accessing RefinedType.
            refinedTypes.foreach(_.takesTypeArgs)
            refinedTypes.flatMap(x => getSelfTypes(x.typeSymbol.asClass))

          // self-type is sometimes just a refined type
          case RefinedType(_ :: types :: Nil, _) =>
            getSelfTypes(types.typeSymbol.asClass)

          // there was no self type
          case x => Nil
        }
      }
    }

    val mixins = additionalTypes.map(_.tree.tpe).collect {
      case ConstantType(Constant(tpe: Type)) => tpe.typeSymbol.asClass
    } flatMap(x => getSelfTypes(x))

    val allSelfTypes = getSelfTypes(c.weakTypeOf[T].typeSymbol.asClass) ++ mixins

    // Gets the distinct self-types.  Unfortunately simply calling .distinct does not seem to work.
    val selfTypes = allSelfTypes.foldLeft(List[c.universe.ClassSymbol](), Set[String]()) {
        case ((lst, set), elem) if !set.contains(elem.fullName) => (elem :: lst, set + elem.fullName)
        case (accum, _) => accum
      }._1.reverse

    val selfTypeIdents = selfTypes.map(x => Ident(x))

    c.echo(c.enclosingPosition, s"Generated class: \nclass Anon \n  extends ${selfTypes.head.fullName}\n  with "
      + selfTypes.tail.map(_.fullName).mkString("\n  with ") + "\nat: ")

    selfTypes.foreach { selfType =>
      def msg: String = selfType.fullName + " in transitive closure.\nCannot add to generated class at:"

      if (selfType.asClass.isSealed) {
        c.error(c.enclosingPosition, "Found sealed trait " + msg)
      }
      if (selfType.asClass.isPrivate) {
        c.error(c.enclosingPosition, "Found private trait " + msg)
      }
    }

    // Creates scala code that looks like the following:
    // {
    //   class $anon extends T with self-types { }
    //   new $anon
    // }
    val block =
      Block(
        stats = List(
          ClassDef(
            Modifiers(Flag.FINAL),
            newTypeName("$anon"),
            List(),
            Template(
              selfTypeIdents,
              emptyValDef,
              List(
                DefDef(
                  Modifiers(),
                  nme.CONSTRUCTOR,
                  List(),
                  List(List()),
                  TypeTree(),
                  Block(
                    stats = List(
                      Apply(
                        Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR),
                        List()
                      )
                    ),
                    expr = Literal(Constant(()))
                  )
                )
              )
            )
          )
        ),
        expr = Apply(Select(New(Ident(newTypeName("$anon"))), nme.CONSTRUCTOR), List()))

    c.Expr[T](block)
  }
  // scalastyle:on

}
