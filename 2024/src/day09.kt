import java.io.File

fun main() = solve("09", ::parse, ::part1, ::part2)

private fun parse(file: File): List<Int> {
    return file.readLines().first().map { c -> c - '0' }
}

private const val EMPTY = -1

private fun part1(input: List<Int>): Long {
    val memory = makeMemoryArray(input)
    fillMemory(input, memory)
    return memory.withIndex().sumOf { (i, fileNum) -> (i * fileNum).toLong() }
}

private fun makeMemoryArray(input: List<Int>): IntArray {
    val totalFileSize = input.withIndex().sumOf { (i, size) ->
        // take just those sizes at even indices
        ((i + 1) % 2) * size
    }

    val memory = IntArray(totalFileSize) { EMPTY }
    var memoryLocation = 0
    for ((i, size) in input.withIndex()) {
        if (i % 2 == 1) { // this is a gap
            memoryLocation += size
            continue
        }
        // this is a file
        val fileNum = i / 2
        repeat(size) {
            if (memoryLocation >= memory.size) return memory
            memory[memoryLocation] = fileNum
            memoryLocation++
        }
    }

    throw IllegalStateException("Got through entire disk map. Are all the gaps zero-sized?")
}

private fun fillMemory(input: List<Int>, memory: IntArray) {
    val filesFromEnd = input.withIndex().filter {
            (i, _) -> i % 2 == 0
    }.map { (i, size) ->
        IndexedValue(i / 2, size)
    }.reversed()
    var memoryLocation = 0
    for ((fileNum, size) in filesFromEnd) {
        repeat(size) {
            while (memory[memoryLocation] != EMPTY) {
                memoryLocation++
                if (memoryLocation >= memory.size) return
            }
            memory[memoryLocation] = fileNum
            memoryLocation++
            if (memoryLocation >= memory.size) return
        }
    }

    throw IllegalStateException("Got through entire disk map. Are all the gaps zero-sized?")
}

// testing function to display the state of memory the same way it's displayed in the AoC problem statement
private fun displayMemory(memory: IntArray): String {
    return memory.joinToString("") { n ->
        if (n == EMPTY) "." else n.toString()
    }
}

data class FileSection(var start: Int, val length: Int)
data class GapSection(var start: Int, var length: Int)

private fun part2(input: List<Int>): Long {
    val (files, gaps) = findSections(input)
    files.reverse()
    for (file in files) {
        for (gap in gaps) {
            if (gap.start >= file.start) break
            if (gap.length >= file.length) {
                file.start = gap.start
                gap.start += file.length
                gap.length -= file.length
                break
            }
        }
    }
    files.reverse()
    return files.withIndex().sumOf { (fileNum, file) ->
        fileNum.toLong() * (file.start..<(file.start + file.length)).sum().toLong()
    }
}

private fun findSections(input: List<Int>): Pair<MutableList<FileSection>, List<GapSection>> {
    val files = mutableListOf<FileSection>()
    val gaps = mutableListOf<GapSection>()
    var memoryPosition = 0
    for ((i, size) in input.withIndex()) {
        if (i % 2 == 0) {
            files.addLast(FileSection(memoryPosition, size))
        } else {
            gaps.addLast(GapSection(memoryPosition, size))
        }
        memoryPosition += size
    }
    return Pair(files, gaps)
}
