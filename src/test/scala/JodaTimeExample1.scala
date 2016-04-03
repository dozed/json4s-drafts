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

  implicit val instantJson = JSON.of[Long].xmap[Instant](x => new Instant(x), i => i.getMillis)

  val dateTimeFormat = ISODateTimeFormat.dateTime.withOffsetParsed

  implicit val durationWrite = JSON.write[Duration] { d =>
    JInt(d.getMillis)
  }

  implicit val dateTimeWrite = JSON.write[DateTime] { d =>
    JString(dateTimeFormat.print(d))
  }

  implicit val intervalWrite = JSON.write[Interval] { i =>
    ("start" -> i.getStartMillis) ~
      ("end" -> i.getEndMillis)
  }

  implicit val localDateWrite = JSON.write[LocalDate] { d =>
    ("year" -> d.getYear) ~
      ("month" -> d.getMonthOfYear) ~
      ("day" -> d.getDayOfMonth)
  }

  implicit val localTimeWrite = JSON.write[LocalTime] { t =>
    ("hour" -> t.getHourOfDay) ~
      ("minute" -> t.getMinuteOfHour) ~
      ("second" -> t.getSecondOfMinute) ~
      ("millis" -> t.getMillisOfSecond)
  }

  implicit val periodWrite = JSON.write[Period] { p =>
    JString(p.toString)
  }

  implicit val durationRead = JSON.readL[Int] map (x => new Duration(x))

  implicit val intervalRead = JSON.read[Interval] { json =>
    (
      (json \ "start").validate[Long] |@|
      (json \ "end").validate[Long]
    ).apply {
      case (start, end) => new Interval(start, end)
    }
  }

  implicit val dateTimeRead = JSON.read[DateTime] { json =>
    json.validate[String] flatMap  { str =>
      try  {
        dateTimeFormat.parseDateTime(str).successNel[Error]
      } catch {
        case t => Fail.apply[DateTime]("", s"Could not parse losless date from: $str")
      }
    }
  }

  implicit val localDateRead = JSON.read[LocalDate] { json =>
    (
      (json \ "year").validate[Int] |@|
      (json \ "month").validate[Int] |@|
      (json \ "day").validate[Int]
    ).apply {
      case (year, month, day) => new LocalDate(year, month, day)
    }
  }

  implicit val localTimeRead = JSON.read[LocalTime] { json =>
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

  implicit val datesJson = deriveJSON[Dates]


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
