import java.io.File

fun main() = solve("25", ::parse, ::part1)

private typealias KeyLock = List<Int>
private data class Input25(val keys: Collection<KeyLock>, val locks: Collection<KeyLock>)

private fun parse(file: File): Input25 {
    val diagrams = file.readLines().chunked(8)
    val (keyDiagrams, lockDiagrams) = diagrams.partition { lines ->
        require(lines.dropLastView().all { line -> line.length == 5 })
        lines.first().first() == '.'
    }
    val keys = keyDiagrams.map { diagram ->
        (0..4).map { column ->
            (1..6).first { row ->
                diagram[row][column] == '#'
            } - 1
        }
    }
    val locks = lockDiagrams.map { diagram ->
        (0..4).map { column ->
            (1..6).first { row ->
                diagram[row][column] == '.'
            } - 1
        }
    }
    return Input25(keys, locks)
}

private fun part1(input: Input25): Int {
    val (keys, locks) = input
    return keys.sumOf { key ->
        locks.count { lock ->
            (key zip lock).all { (keyHeight, lockHeight) -> keyHeight >= lockHeight }
        }
    }
}
