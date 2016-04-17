import org.json4s._
import org.json4s.jackson._
import org.json4s.ext.scalaz.JsonScalaz._

import scalaz._, Scalaz._

object JodaTimeExample1 extends App {

  import org.joda.time.Duration
  import org.joda.time.Instant
  import org.joda.time.DateTime
  import org.joda.time.Interval
  import org.joda.time.LocalDate
  import org.joda.time.LocalTime
  import org.joda.time.Period
  import org.joda.time.format.ISODateTimeFormat


  // define JSONW and JSONR instances for joda-time types

  implicit val instantJson = JSON[Long].xmap[Instant](x => new Instant(x), i => i.getMillis)

  val dateTimeFormat = ISODateTimeFormat.dateTime.withOffsetParsed

  implicit val durationWrite = JSONW.instance[Duration] { d =>
    JInt(d.getMillis)
  }

  implicit val dateTimeWrite = JSONW.instance[DateTime] { d =>
    JString(dateTimeFormat.print(d))
  }

  implicit val intervalWrite = JSONW.instance[Interval] { i =>
    ("start" -> i.getStartMillis) ~
      ("end" -> i.getEndMillis)
  }

  implicit val localDateWrite = JSONW.instance[LocalDate] { d =>
    ("year" -> d.getYear) ~
      ("month" -> d.getMonthOfYear) ~
      ("day" -> d.getDayOfMonth)
  }

  implicit val localTimeWrite = JSONW.instance[LocalTime] { t =>
    ("hour" -> t.getHourOfDay) ~
      ("minute" -> t.getMinuteOfHour) ~
      ("second" -> t.getSecondOfMinute) ~
      ("millis" -> t.getMillisOfSecond)
  }

  implicit val periodWrite = JSONW.instance[Period] { p =>
    JString(p.toString)
  }

  implicit val durationRead = JSONR[Int] map (x => new Duration(x))

  implicit val intervalRead = JSONR.instance[Interval] { json =>
    (
      (json \ "start").validate[Long] |@|
      (json \ "end").validate[Long]
    ).apply {
      case (start, end) => new Interval(start, end)
    }
  }

  implicit val dateTimeRead = JSONR.instance[DateTime] { json =>
    json.validate[String] flatMap  { str =>
      try  {
        dateTimeFormat.parseDateTime(str).successNel[Error]
      } catch {
        case t => Fail.apply[DateTime]("", s"Could not parse losless date from: $str")
      }
    }
  }

  implicit val localDateRead = JSONR.instance[LocalDate] { json =>
    (
      (json \ "year").validate[Int] |@|
      (json \ "month").validate[Int] |@|
      (json \ "day").validate[Int]
    ).apply {
      case (year, month, day) => new LocalDate(year, month, day)
    }
  }

  implicit val localTimeRead = JSONR.instance[LocalTime] { json =>
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

  implicit val datesJson = JSON.derive[Dates]


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

  val res = parseJson(testJson2).validate[Dates]
  println(res)

}
