package drafts

object Macros {

  import scala.reflect.macros.blackbox

  import org.json4s.JValue
  import org.json4s.scalaz.JsonScalaz._

  def typeTagGenImpl[T: c.WeakTypeTag](c: blackbox.Context): c.Expr[T => String] = {
    import c.universe._
    val tpe = weakTypeOf[T].typeSymbol
    tpe.typeSignature // SI-7046 workaround

    val names = tpe.asClass.knownDirectSubclasses.map { x =>
      cq"""x:${x.asClass} => ${x.fullName}"""
    }.toList

    c.Expr[T => String](q"""(t: ${tpe.asClass}) => t match { case ..$names }""")
  }

  def writerGenImpl[T: c.WeakTypeTag](c: blackbox.Context): c.Expr[JSONW[T]] = {
    import c.universe._
    val tpe = weakTypeOf[T]

    val primaryConstructor = tpe.decls.collectFirst{
      case m: MethodSymbol if m.isPrimaryConstructor => m
    }

    primaryConstructor match {
      case Some(constructor) => {
        val fields = constructor.paramLists.flatten.map {field =>
          c.Expr[(String, JValue)](q"""(${field.name.decodedName.toString}, implicitly[org.json4s.scalaz.JsonScalaz.JSONW[${field.typeSignature}]].write(t.${field.name.toTermName}))""")
        }

        c.Expr[JSONW[T]](
          q"""
           new org.json4s.scalaz.JsonScalaz.JSONW[$tpe] {
             override def write(t: $tpe) = org.json4s.JObject(List(..$fields))
           }
           """
        )
      }
      case None => c.abort(c.enclosingPosition, "Could not identify primary constructor for " + tpe)
    }
  }


}
