import java.io.File

fun <T> solve(day: String, parse: (File) -> T, part1: ((T) -> Int)? = null, part2: ((T) -> Int)? = null) {
    val testData = parse(File("test_input/$day.txt"))
    val inputData = parse(File("input/$day.txt"))
    if (part1 != null) {
        println("""
            part 1 test: ${ part1(testData) }
            part 1 real: ${ part1(inputData) }
        """.trimIndent())
    }
    if (part2 != null) {
        println("""
            part 2 test: ${ part2(testData) }
            part 2 real: ${ part2(inputData) }
        """.trimIndent())
    }
}
