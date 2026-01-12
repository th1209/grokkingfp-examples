package MyChapter06

case class TvShow(title: String, start: Int, end: Int)

object myChapter06Main extends App {
    {
        def extractYearTitle(rawShow: String): Option[String] = {
            val bracketOpen = rawShow.indexOf('(')
            if (bracketOpen > 0)
                Some(rawShow.substring(0, bracketOpen).trim)
            else
                None
        }

        def extractYearStart(rawShow: String): Option[Int] = {
            val bracketOpen = rawShow.indexOf('(')
            val dash = rawShow.indexOf('-')
            val yearStrOpt = 
                if (bracketOpen != -1 && dash > bracketOpen + 1)
                    Some(rawShow.substring(bracketOpen + 1, dash).trim)
                else
                    None
            // mapのみだとOption[Option[Int]]が返るためコンパイルエラー
            // (一見難しいが、Listなどに置き換えて考えてみればわかるはず)
            yearStrOpt.map(so => so.toIntOption).flatten
        }

        def extractYearEnd(rawShow: String): Option[Int] = {
            // YearStartとは異なり、for内包表記で書いてみた
            val dash = rawShow.indexOf('-')
            val bracketEnd = rawShow.indexOf(')')
            for {
                yearStrOpt <- if (dash != -1 && bracketEnd > dash + 1) Some(rawShow.substring(dash + 1, bracketEnd).trim) else None
                year <- yearStrOpt.toIntOption
            } yield year
        }

        def extractSingleYear(rawShow: String): Option[Int] = {
            val dash = rawShow.indexOf('-')
            val bracketStart = rawShow.indexOf('(')
            val bracketEnd = rawShow.indexOf(')')
            for {
                yearOpt <- if (dash == -1 && bracketStart != -1 && bracketEnd > bracketStart + 1) Some(rawShow.substring(bracketStart + 1, bracketEnd).trim) else None
                year <- yearOpt.toIntOption
            } yield year
        }

        def parseShow(rawShow: String): Option[TvShow] = {
            for {
                title <- extractYearTitle(rawShow)
                yearStart <- extractYearStart(rawShow).orElse(extractSingleYear(rawShow))
                yearEnd <- extractYearEnd(rawShow).orElse(extractSingleYear(rawShow))
            } yield TvShow(title, yearStart, yearEnd)
        }

        def addOrResign(parsedShows: Option[List[TvShow]], addOne: Option[TvShow]): Option[List[TvShow]] = {
            for {
                shows <- parsedShows
                parsedShow <- addOne
            } yield shows.appended(parsedShow)
        }

        // ベストエフェート型
        def parseShows(rawShows: List[String]) : List[TvShow] = 
            rawShows
                .map(parseShow)
                .map(_.toList) // Option[T] -> List[T]
                .flatten

        // オールオアナッシング型
        def parseShowsRestrict(rawShows: List[String]) : Option[List[TvShow]] = 
            val initialList: Option[List[TvShow]] = Some(List.empty)
            rawShows
                .map(parseShow)
                .foldLeft(initialList)(addOrResign)

        def sortTvShows(shows: List[TvShow]) : List[TvShow] = 
            // 放送期間の長い順
            shows.sortBy(s => s.end - s.start).reverse

        println("== parse movie str ==")
        println(parseShows(List(
            "Breaking Bad ( 2008 - 2013 )",
            "Mad Men (-2015)",
            "(2002- N/A ) The Wire",
            "Chernobyl(2019)",
        )))

        println("== all or nothing error handling strategy ==")
        println(parseShowsRestrict(List("Chernobyl(2019)")))
        println(parseShowsRestrict(List("Chernobyl(2019)", "Breaking Bad")))
        println(parseShowsRestrict(List.empty))
    }

    {
        def extractYearTitle(rawShow: String): Either[String, String] = {
            val bracketOpen = rawShow.indexOf('(')
            if (bracketOpen > 0)
                Right(rawShow.substring(0, bracketOpen).trim)
            else
                Left(s"Can't extract title from $rawShow")
        }

        def extractYearStart(rawShow: String): Either[String, Int] = {
            val bracketOpen = rawShow.indexOf('(')
            val dash = rawShow.indexOf('-')
            for {
                yearStr <- 
                    if (bracketOpen != -1 && dash > bracketOpen + 1)
                        Right(rawShow.substring(bracketOpen + 1, dash).trim)
                    else
                        Left(s"Can't extract start year from $rawShow")
                year <- yearStr.toIntOption.toRight(s"Can't parse $yearStr")
            } yield year
        }

        def extractYearEnd(rawShow: String): Either[String, Int] = {
            val dash = rawShow.indexOf('-')
            val bracketEnd = rawShow.indexOf(')')
            for {
                yearStr <-
                    if (dash != -1 && bracketEnd > dash + 1)
                        Right(rawShow.substring(dash + 1, bracketEnd).trim)
                    else
                        Left(s"Can't extract end year from $rawShow")
                year <- yearStr.toIntOption.toRight(s"Can't parse $yearStr")
            } yield year
        }

        def extractSingleYear(rawShow: String): Either[String, Int] = {
            val dash = rawShow.indexOf('-')
            val bracketStart = rawShow.indexOf('(')
            val bracketEnd = rawShow.indexOf(')')
            for {
                yearStr <-
                    if (dash == -1 && bracketStart != -1 && bracketEnd > bracketStart + 1)
                        Right(rawShow.substring(bracketStart + 1, bracketEnd).trim)
                    else
                        Left(s"Can't extract single year from $rawShow")
                year <- yearStr.toIntOption.toRight(s"Can't parse $yearStr")
            } yield year
        }

        def parseShow(rawShow: String): Either[String, TvShow] = {
            for {
                title <- extractYearTitle(rawShow)
                yearStart <- extractYearStart(rawShow).orElse(extractSingleYear(rawShow))
                yearEnd <- extractYearEnd(rawShow).orElse(extractSingleYear(rawShow))
            } yield TvShow(title, yearStart, yearEnd)
        }

        def addOrResign(parsedShows: Either[String, List[TvShow]], addOne: Either[String, TvShow]): Either[String, List[TvShow]] = {
            for {
                shows <- parsedShows
                parsedShow <- addOne
            } yield shows.appended(parsedShow)
        }

        def parseShows(rawShows: List[String]) : Either[String, List[TvShow]] = 
            val initialList: Either[String, List[TvShow]] = Right(List.empty)
            rawShows
                .map(parseShow)
                .foldLeft(initialList)(addOrResign)

        println("== use either class ==")
        println(parseShows(List("Chernobyl(2019)")))
        println(parseShows(List("Chernobyl(2019)", "Breaking Bad")))
        println(parseShows(List.empty))
    }
}
