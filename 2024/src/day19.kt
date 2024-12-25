import java.io.File

fun main() = solve("19", ::parse, ::part1, ::part2)

private data class Input19(val towels: List<String>, val designs: List<String>)
private fun parse(file: File): Input19 {
    val lines = file.readLines()
    val towels = lines.first().split(", ")
    val designs = lines.drop(2)
    return Input19(towels, designs)
}

private fun part1(input: Input19): Int {
    val (towels, designs) = input
    val isDesignPossible = Memoize<String, Boolean> { design ->
        if (design.isEmpty()) return@Memoize true
        towels.any { towel ->
            design.startsWith(towel) && this(design.removePrefix(towel))
        }
    }
    return designs.count(isDesignPossible)
}

private fun part2(input: Input19): Long {
    val (towels, designs) = input
    val numPossibleArrangements = Memoize<String, Long> { design ->
        if (design.isEmpty()) return@Memoize 1
        towels.sumOf { towel ->
            if (design.startsWith(towel)) {
                this(design.removePrefix(towel))
            } else 0
        }
    }
    return designs.sumOf(numPossibleArrangements)
}
