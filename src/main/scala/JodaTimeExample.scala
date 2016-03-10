

object JodaTimeExample extends App {

  import org.json4s._
  import org.json4s.jackson._
  import org.json4s.scalaz.JsonScalaz._
  import drafts.WriteExt._
  import drafts.ReadExt._
  import _root_.scalaz._, Scalaz._

  import org.joda.time.Duration
  import org.joda.time.Instant
  import org.joda.time.DateTime
  import org.joda.time.Interval
  import org.joda.time.LocalDate
  import org.joda.time.LocalTime
  import org.joda.time.Period
  import org.joda.time.format.ISODateTimeFormat

  val dateTimeFormat = {
    ISODateTimeFormat.dateTime.withOffsetParsed
  }

  // JSONW[A] writer typeclass from A => JValue function

  implicit val durationWrite: JSONW[Duration] = write[Duration] { d =>
    JInt(d.getMillis)
  }

  implicit val instantWrite: JSONW[Instant] = write[Instant] { i =>
    JInt(i.getMillis)
  }

  implicit val dateTimeWrite = write[DateTime] { d =>
    JString(dateTimeFormat.print(d))
  }

  implicit val intervalWrite = write[Interval] { i =>
    ("start" -> i.getStartMillis) ~
      ("end" -> i.getEndMillis)
  }

  implicit val localDateWrite = write[LocalDate] { d =>
    ("year" -> d.getYear) ~
      ("month" -> d.getMonthOfYear) ~
      ("day" -> d.getDayOfMonth)
  }

  implicit val localTimeWrite = write[LocalTime] { t =>
    ("hour" -> t.getHourOfDay) ~
      ("minute" -> t.getMinuteOfHour) ~
      ("second" -> t.getSecondOfMinute) ~
      ("millis" -> t.getMillisOfSecond)
  }

  implicit val periodWrite = write[Period] { p =>
    JString(p.toString)
  }


  // JSONR[A] reader typeclass from JValue => ValidationNel[Error, A] function

  implicit val durationRead: JSONR[Duration] = jsonr[Duration] { json =>
    json.validate[Int] map (x => new Duration(x))
  }

  implicit val instantRead: JSONR[Instant] = jsonr[Instant] { json =>
    json.validate[Int] map (x => new Instant(x))
  }

  implicit val intervalRead: JSONR[Interval] = jsonr[Interval] { json =>
    (
      (json \ "start").validate[Long] |@|
      (json \ "end").validate[Long]
    ).apply {
      case (start, end) => new Interval(start, end)
    }
  }

  implicit val dateTimeRead: JSONR[DateTime] = jsonr[DateTime] { json =>
    json.validate[String] flatMap  { str =>
      try  {
        dateTimeFormat.parseDateTime(str).successNel[Error]
      } catch {
        case t => Fail.apply[DateTime]("", s"Could not parse losless date from: $str")
      }
    }
  }

  implicit val localDateRead: JSONR[LocalDate] = jsonr[LocalDate] { json =>
    (
      (json \ "year").validate[Int] |@|
      (json \ "month").validate[Int] |@|
      (json \ "day").validate[Int]
    ).apply {
      case (year, month, day) => new LocalDate(year, month, day)
    }
  }

  implicit val localTimeRead: JSONR[LocalTime] = jsonr[LocalTime] { json =>
    (
      (json \ "hour").validate[Int] |@|
      (json \ "minute").validate[Int] |@|
      (json \ "second").validate[Int] |@|
      (json \ "millis").validate[Int]
    ).apply {
      case (hour, minute, seconds, millis) => new LocalTime(hour, minute, seconds, millis)
    }
  }



  // LocalDate

  // write LocalDate to json
  val json1 = LocalDate.now().toJson

  // read LocalDate from json
  val localDate1 = json1.validate[LocalDate]


  println(json1)
  // JObject(List((year,JInt(2016)), (month,JInt(3)), (day,JInt(10))))

  println(localDate1)
  // Success(2016-03-10)




  // all together

  case class Dates(
    duration: Duration,
    instant: Instant,
    interval: Interval,
    dateTime: DateTime,
    localDate: LocalDate,
    localTime: LocalTime
  )


  implicit val datesWrite = writerGen[Dates]


  val testJson = Dates(new Duration(10), new Instant(10), new Interval(10, 200), DateTime.now(), DateTime.now().toLocalDate, DateTime.now().toLocalTime).toJson

  println(prettyJson(testJson))


  val testJson2 =
    """
      |{
      |  "duration" : 10,
      |  "instant" : 10,
      |  "interval" : {
      |    "start" : 10,
      |    "end" : 200
      |  },
      |  "dateTime" : "2016-03-10T01:33:04.887+01:00",
      |  "localDate" : {
      |    "year" : 2016,
      |    "month" : 3,
      |    "day" : 10
      |  },
      |  "localTime" : {
      |    "hour" : 1,
      |    "minute" : 33,
      |    "second" : 4,
      |    "millis" : 893
      |  }
      |}
    """.stripMargin

  // TODO
  //  val res = parseJson(testJson2).validate[Dates]
  //  println(res)

}
