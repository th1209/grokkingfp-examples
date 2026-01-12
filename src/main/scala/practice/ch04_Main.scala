package MyChapter04

def len(s: String): Int = s.length()

def numberOfS(s: String) : Int = s.length() - s.replaceAll("s", "").replaceAll("S", "").length()

def negative(i: Int) : Int = -1 * i

def double(i: Int) : Int = i * 2

def odd(i: Int) : Boolean = (i % 2).abs == 1

def even(i: Int) : Boolean = i % 2 == 0

// 以下 高階関数の例
def divisibleBy(n: Int) : Int => Boolean = i => i % n == 0

def shorterThan(n: Int) : String => Boolean = s => s.length < n

// 以下 カリー化の例
def divisibleBy(n: Int)(i : Int) : Boolean = i % n == 0

def largerThan(n: Int)(i : Int) : Boolean = i > n

def containsS(moreThan: Int)(s: String): Boolean = numberOfS(s) > moreThan

// 以下、直積型(product type)の例
case class ProgrammingLanguage(name: String, year: Int)

object myChapter04Main extends App {
    {
        val languages = List("C++", "C#", "Scala", "Rust")
        val numbers = List(5, 1, -2, 0, 3, -1)
        
        println("== map example==")
        println(languages.map(len))
        println(languages.map(numberOfS))

        println("== filter example==")
        println(languages.filter(l => len(l) < 5))
        println(languages.filter(l => numberOfS(l) > 0))
        println(numbers.filter(odd))
        println(numbers.filter(n => n > 2))
        println(-3 % 2)

        println("== higher order function example==")
        println(languages.filter(shorterThan(4)))
        println(numbers.filter(divisibleBy(5)))

        println("== currying example==")
        println(numbers.filter(divisibleBy(5)))
        println(languages.filter(containsS(0)))

        println("== foldLeft(accumulator) example==")
        println(List(1,2,3,4,5).foldLeft(0)((sum, i) => sum + i))
        println(List("apple", "banana", "peach").foldLeft(0)((sum, s) => sum + s.length()))
        println(List("scala", "haskell", "rust", "ada").foldLeft(0)((sum, s) => sum + numberOfS(s)))
        println(List(5, 1, 2, 4, 15).foldLeft(0)((max, n) => if (max > n) max else n))
    }

    {
        println("== product type example==")
        val javalang = ProgrammingLanguage("Java", 1995)
        val scalalang = ProgrammingLanguage("Scala", 2004)
        val languages = List(javalang, scalalang)
        println(languages.map(l => l.name))
        // 以下アンダースコア構文と呼ばれる省略記法
        println(languages.map(_.name))
        println(languages.filter(_.year > 2000))
    }
}
