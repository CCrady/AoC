import java.io.File

fun main() = solve("19", ::parse, ::part1)

private data class Input19(val towels: List<String>, val designs: List<String>)

private fun parse(file: File): Input19 {
    val lines = file.readLines()
    val towels = lines.first().split(", ")
    val designs = lines.drop(2)
    return Input19(towels, designs)
}

private fun part1(input: Input19): Int {
    val (inputTowels, designs) = input
    val optimizedTowels = optimizeTowels(inputTowels)
    return designs.count { design -> isDesignPossible(design, optimizedTowels) }
}

private fun optimizeTowels(towels: List<String>): List<String> {
    val towelSet = towels.toSet()
    return towelSet.filter { towel ->
        !isDesignPossible(towel, towelSet.minusElement(towel))
    }.sortedByDescending(String::length)
}

fun isDesignPossible(design: String, towels: Collection<String>): Boolean {
    if (design.isEmpty()) return true
    return towels.any { towel ->
        design.startsWith(towel) && isDesignPossible(design.removePrefix(towel), towels)
    }
}
