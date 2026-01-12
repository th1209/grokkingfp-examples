package MyChapter05

case class Book(title: String, authors: List[String])

case class Movie(title: String)

case class Point(x: Double, y: Double)

def isInside(point: Point, radius: Double) : Boolean = 
    radius * radius >= point.x * point.x + point.y * point.y

case class Event(name: String, start: Int, end: Int)

// Option型を返す関数、直接書いてみた場合
// def parseEvent(name: String, start: Int, end: Int): Option[Event] =
// {
//     if (name.size > 0 && start <= end && end < 3000)
//         Some(Event(name, start, end))
//     else
//         None
// }
def validateEventName(name: String) : Option[String] = if (name.size > 0) Some(name) else None
def validateEventStart(start: Int, end: Int) : Option[Int] = if (start <= end) Some(start) else None
def validateEventEnd(end: Int) : Option[Int] = if (end < 3000) Some(end) else None
def parseEvent(name: String, start: Int, end: Int): Option[Event] =
    // for内包表記は、いずれかの列挙しがNoneを返す場合、Noneを返してくれる
    for {
        validName  <- validateEventName(name)
        validStart <- validateEventStart(start, end)
        validEnd   <- validateEventEnd(end)
    } yield Event(validName, validStart, validEnd)

object myChapter05Main extends App {
    {
        def bookAdaptations(author: String) : List[Movie] =
        {
            if (author == "Tolkien")
            {
                List(
                    Movie("An Unexpected Journey"),
                    Movie("The Desolation of Smaug"),
                )
            }
            else
            {
                List.empty
            }
        }

        val books = List(
            Book("FP in Scala", List("Chiusano", "Bjarnason")),
            Book("The Hobbit", List("Tolkien")),
        )

        println("== flatMap example==")
        println(books.flatten(_.authors))
        // println(books.flatMap(_.authors).map(bookAdaptations))
        println(books.flatMap(_.authors).flatMap(bookAdaptations))
    }

    {
        println("== flatMap example2==")
        println(List(1,2,3).flatMap(i => List(i, i + 10)))
        println(List(1,2,3).flatMap(i => List(i * 2)))
        println(List(1,2,3).flatMap(i => if (i % 2 == 0) List(i) else List.empty))
    }

    {
        println("== flatMap example3==")
        def recommendedBooks(friend: String) : List[Book] =
        {
            val scala = List(
                Book("FP in Scala", List("Chiusano", "Bjarnason")),
                Book("Get Programming with Scala", List("Sfregola")),
            )
            val fiction = List(
                Book("Harry Potter", List("Rowling")),
                Book("The Load of the Rings", List("Tolkien")),
            )

            if (friend == "Alice") scala
            else if (friend == "Bob") fiction
            else List.empty
        }

        val friends = List("Alice", "Bob", "Charlie")
        val recommendations = friends.flatMap(recommendedBooks).flatMap(_.authors)
        println(recommendations)
    }

    {
        def bookAdaptations(author: String) : List[Movie] =
        {
            if (author == "Tolkien")
            {
                List(
                    Movie("An Unexpected Journey"),
                    Movie("The Desolation of Smaug"),
                )
            }
            else
            {
                List.empty
            }
        }

        val books = List(
            Book("FP in Scala", List("Chiusano", "Bjarnason")),
            Book("The Hobbit", List("Tolkien")),
        )

        println("== flatMap nest==")
        // flatMapのネスト、末尾はmapで良い点に注意(試しに末尾をflatMapにすると結果が変わるので試してみよう)
        var recommendation = books.flatMap(book => 
            book.authors.flatMap(author => 
                bookAdaptations(author).map(movie =>
                    s"You may like ${movie.title}, " + 
                    s"because you liked ${author}'s ${book.title}"
                )
            )
        )
        println(recommendation)
        // flatMapのネストには、for内包表記という糖衣構文がある
        recommendation = for {
            book <- books
            author <- book.authors
            movie <- bookAdaptations(author)
        } yield s"You may like ${movie.title}, because you liked ${author}'s ${book.title}"
        println(recommendation)

        val points = List(Point(5,2), Point(1, 1))
        val radiuses = List(2, 1)
        var results = for {
            radius <- radiuses
            point <- points
        } yield s"$point is within a radius of $radius: ${isInside(point, radius)}"
        println(results)

        def insideFilter(point: Point, radius: Double): List[Point] =
            if (isInside(point, radius)) List(point) else List.empty
        results = for {
            radius <- radiuses
            point <- points
            inPoint <- insideFilter(point, radius)
        } yield s"$inPoint is within a radius of $radius"
        println(results)
    }

    {
        // for内包表記の出力型は、最初の列挙子に指定した型で決まる　
        println("==list comprehension output data type==")
        println(
            for {
                a <- List(1, 2)
                b <- Set(2, 1)
            } yield a * b
        )
        println(
            for {
                a <- Set(2, 1)
                b <- List(1, 2)
            } yield a * b
        )
    }

    {
        println("==option type==")
        println(parseEvent("Apollo Program", 1961, 1972))
        println(parseEvent("Invalid Event", 0, 3000))
    }
}

