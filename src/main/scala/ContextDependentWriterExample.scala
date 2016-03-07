
object ContextDependentWriterExample extends App {

  import org.json4s.jackson._
  import drafts.WriteExt._

  case class Document(id: String)
  case class Item(id: String, text: String)
  case class User(id: String)

  case class ItemCreatedMessage(item: Item, author: User)


  // context-free writers

  implicit val itemWriter = JsonWriter[Item] { item =>
    ("id" -> item.id) ~
      ("text" -> item.text)
  }

  implicit val userWriter = JsonWriter[User] { user =>
    ("id" -> user.id)
  }


  // context-dependent writers

  // example:
  // - an ItemCreatedMessage should write the author only, if the message is targetted to the author
  // - all other recipients should not see an author

  type MessagePrivacy = String
  type ParentDocument = Document

  implicit val itemCreatedMessage = JsonWriter.context[(MessagePrivacy, ParentDocument), ItemCreatedMessage] { case ((privacy, document), msg) =>

    if (privacy == "public") {

      ("type" -> "ItemCreated") ~
        ("item" -> msg.item)

    } else {

      ("type" -> "ItemCreated") ~
        ("item" -> msg.item) ~
        ("author" -> msg.author)

    }


  }



  // standard flow: handling a request, create a new item, yields a ItemCreatedMessage

  val author = User("28374")
  val document = Document("12983")
  val item = Item("3827", "an item")

  val msg = ItemCreatedMessage(item, author)



  // now two different messages can be constructed, one to return to the author, one to return the public
  // - authorMsg can be returned in the response
  // - publicMsg can be pushed via sockets to other clients

  val authorMsg = msg.toJson(("author", document))
  //  {
  //    "type" : "ItemCreated",
  //    "item" : {
  //      "id" : "3827",
  //      "text" : "an item"
  //    },
  //    "author" : {
  //      "id" : "28374"
  //    }
  //  }

  val publicMsg = msg.toJson(("public", document))
  //  {
  //    "type" : "ItemCreated",
  //    "item" : {
  //      "id" : "3827",
  //      "text" : "an item"
  //    }
  //  }


  println(prettyJson(authorMsg))
  println(prettyJson(publicMsg))


}
