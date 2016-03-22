package drafts

import scala.reflect.macros.whitebox
import org.json4s.JValue
import org.json4s.ext.scalaz.JsonScalaz._

import macrocompat.bundle

object Macros {

  import scala.language.experimental.macros

  def typeTagGen[A]: A => String = macro Macros.typeTagGenImpl[A]

  def writerGen[A]: JSONW[A] = macro Macros.writerGenImpl[A]

}

@bundle
class Macros(val c: whitebox.Context) {

  import c.universe._

  def typeTagGenImpl[T: c.WeakTypeTag]: Tree = {
    import c.universe._
    val tpe = weakTypeOf[T].typeSymbol
    tpe.typeSignature // SI-7046 workaround

    val names = tpe.asClass.knownDirectSubclasses.map { x =>
      cq"""x:${x.asClass} => ${x.fullName}"""
    }.toList

    q"""(t: ${tpe.asClass}) => t match { case ..$names }"""
  }

  def writerGenImpl[T: c.WeakTypeTag]: Tree = {
    import c.universe._
    val tpe = weakTypeOf[T]

    val primaryConstructor = tpe.decls.collectFirst{
      case m: MethodSymbol if m.isPrimaryConstructor => m
    }

    primaryConstructor match {
      case Some(constructor) => {
        val fields = constructor.paramLists.flatten.map {field =>
          c.Expr[(String, JValue)](q"""(${field.name.decodedName.toString}, implicitly[org.json4s.ext.scalaz.JsonScalaz.JSONW[${field.typeSignature}]].write(t.${field.name.toTermName}))""")
        }

        q"""
         new org.json4s.ext.scalaz.JsonScalaz.JSONW[$tpe] {
           override def write(t: $tpe) = org.json4s.JObject(List(..$fields))
         }
         """
      }
      case None => c.abort(c.enclosingPosition, "Could not identify primary constructor for " + tpe)
    }
  }


}
