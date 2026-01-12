// package MyChapter08

// import practice.MyJavaClass
// import practice.ch08_SchedulingMeetingsAPI
import cats.effect.IO
import cats.implicits.*
import cats.effect.unsafe.implicits.global

import scala.util.Try

// object Hoge {

// case class MeetingTime(startHour: Int, endHour: Int)

def calenderEntriesApiCall(name: String): List[MeetingTime] = {
// def calenderEntriesApiCall(name: String): Unit = {
    import scala.jdk.CollectionConverters._
    // List(MeetingTime(1, 1))
    ch08_SchedulingMeetingsAPI.calendarEntriesApiCall(name).asScala.toList
}

// def createMeetingApiCall(names: List[String], meetingTime: MeetingTime): Unit = {
//     import scala.jdk.CollectionConverters._
//     ch08_SchedulingMeetingsAPI.createMeetingApiCall(names.asJava, meetingTime)
// }

// import java.util.{Date, Locale}
// import java.text.DateFormat._

object myChapter08Main extends App {

    println(calenderEntriesApiCall("Alice"))

    // val now = new Date
    // val df = getDateInstance(LONG, Locale.FRANCE)
    // println(df format now)

    // MyJavaClass.SayHello()

    println("test")
}

// }
