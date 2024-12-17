import java.io.File

fun main() = solve("03", ::parse, ::part1, ::part2)

private fun parse(file: File): String = file.readText()

private fun part1(input: String): Int {
    val pattern = """mul\((\d{1,3}),(\d{1,3})\)""".toRegex()
    return pattern.findAll(input).sumOf { match ->
        val (left, right) = match.destructured
        left.toInt() * right.toInt()
    }
}

private fun part2(input: String): Int {
    val doPattern = """do\(\)""".toRegex()
    val mulPattern = """mul\((\d{1,3}),(\d{1,3})\)""".toRegex()
    val dontPattern = """don't\(\)""".toRegex()
    // split input on uses of do(), then take just the region before the following don't(), then collect the mul()s
    // laziness is magical :D
    return doPattern.splitToSequence(input).map { region ->
        dontPattern.splitToSequence(region).first()
    }.flatMap { region ->
        mulPattern.findAll(region)
    }.sumOf { match ->
        val (left, right) = match.destructured
        left.toInt() * right.toInt()
    }
}