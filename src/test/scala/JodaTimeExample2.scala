import org.json4s._
import org.json4s.ext.scalaz.JsonScalaz._
import org.json4s.jackson._

import scala.util.Try
import scalaz.Scalaz._

object JodaTimeExample2 extends App {

  import org.joda.time._
  import org.joda.time.format.ISODateTimeFormat

  val dateTimeFormat = ISODateTimeFormat.dateTime.withOffsetParsed


  // define JSON instances for joda-time types

  implicit val instantJson: JSON[Instant] = JSON.of[Long].xmap[Instant](x => new Instant(x), i => i.getMillis)

  implicit val durationJson: JSON[Duration] = JSON.of[Long].xmap[Duration](new Duration(_), _.getMillis)

  implicit val dateTimeJson: JSON[DateTime] = JSON.of[String].exmap[DateTime](
    str => {
      Try(dateTimeFormat.parseDateTime(str).successNel[Error]) getOrElse {
        Fail.invalidFormat(s"Could not parse losless date from: $str")
      }
    },
    d => dateTimeFormat.print(d)
  )

  implicit val intervalJson: JSON[Interval] = json2[Interval, Long, Long]("start", "end")(
    (i: Interval) => (i.getStartMillis, i.getEndMillis).some,
    (start, end) => new Interval(start, end)
  )

  implicit val localDateJson: JSON[LocalDate] = json3[LocalDate, Int, Int, Int]("year", "month", "day")(
    (d: LocalDate) => (d.getYear, d.getMonthOfYear, d.getDayOfMonth).some,
    (year, month, day) => new LocalDate(year, month, day)
  )

  implicit val localTimeJson: JSON[LocalTime] = json4[LocalTime, Int, Int, Int, Int]("hour", "minute", "second", "millis")(
    (t: LocalTime) => (t.getHourOfDay, t.getMinuteOfHour, t.getSecondOfMinute, t.getMillisOfSecond).some,
    (hour, minute, seconds, millis) => new LocalTime(hour, minute, seconds, millis)
  )

  implicit val periodWrite: JSON[Period] = JSON.of[String].exmap[Period](
    s => Try(Period.parse(s).successNel) getOrElse Fail.invalidFormat(s"Could not parse Period: $s"),
    p => p.toString
  )


  // all together

  case class DatesCC(
    duration: Duration,
    instant: Instant,
    interval: Interval,
    dateTime: DateTime,
    localDate: LocalDate,
    localTime: LocalTime
  )

  implicit val datesJson = deriveJSON[DatesCC]



  // LocalDate

  val json1 = LocalDate.now().toJson
  val localDate1 = json1.validate[LocalDate]
  println(json1)
  // JObject(List((year,JInt(2016)), (month,JInt(3)), (day,JInt(10))))

  println(localDate1)
  // Success(2016-03-10)



  val testJson2 =
    parseJson("""
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
    """.stripMargin)

  val res = testJson2.validate[DatesCC]
  println(res)

}
