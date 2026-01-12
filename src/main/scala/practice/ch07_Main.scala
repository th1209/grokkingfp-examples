package MyChapter07

// 直和型
enum MusicGenre {
    case HeavyMetal
    case Pop
    case HardRock
}

// newtype
opaque type Location = String
object Location {
    def apply(value: String): Location = value
    extension (a: Location) def name: String = a
}

// opaque type YearsActiveStart = Int
// object YearsActiveStart {
//     def apply(value: Int): YearsActiveStart = value
//     extension (a: YearsActiveStart) def value: Int = a
// }
// opaque type YearsActiveEnd = Int
// object YearsActiveEnd {
//     def apply(value: Int): YearsActiveEnd = value
//     extension (a: YearsActiveEnd) def value: Int = a
// }
// case class PeriodInYears(start: Int, end: Option[Int])

// 代数的データ型(直和型 + 直積型)
enum YearsActive {
    case StillActive(since: Int)
    case ActiveBetween(start: Int, end: Int)
}

case class Artist(
    name: String,
    genre: MusicGenre,
    origin: Location,
    yearsActive: YearsActive,
)

enum SearchCondition {
    case SearchByGenre(genre: List[MusicGenre])
    case SearchByOrigin(locations: List[Location])
    case SearchByActiveYears(start: Int, end: Int)
}

import MusicGenre._, YearsActive._, SearchCondition._

def wasArtistActive(artist: Artist, yearStart: Int, yearEnd: Int) : Boolean = {
    artist.yearsActive.match {
        case StillActive(since) => since <= yearEnd
        case ActiveBetween(start, end) => start <= yearEnd && end >= yearStart
    }
}

def activeLength(artist: Artist, currentYear: Int): Int = 
    artist.yearsActive.match
        case StillActive(since) => currentYear - since
        case ActiveBetween(start, end) => end - start

def searchArtists(artists: List[Artist], requiredConditions: List[SearchCondition]) : List[Artist] = 
    artists.filter(artist =>
        requiredConditions.forall(condition =>
            condition.match {
                case SearchByGenre(genres) => genres.contains(artist.genre)
                case SearchByOrigin(locations) => locations.contains(artist.origin)
                case SearchByActiveYears(start, end) => wasArtistActive(artist, start, end)
            }
        )
    )

object myChapter07Main extends App {
    val artists = List(
        Artist("Metallica", HeavyMetal, Location("U.S."), StillActive(since = 1981)),
        Artist("Led Zeppelin", HardRock, Location("England"), ActiveBetween(1968, 1980)),
        Artist("Bee Gees", Pop, Location("England"), ActiveBetween(1958, 2003))
    )
    println(searchArtists(artists, List(SearchByActiveYears(1950, 2022))))
}

