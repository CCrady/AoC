import java.io.File

fun main() = solve("23", ::parse, ::part1)

private fun parse(file: File): UndirectedGraph<String> {
    val associations = file.readLines().map { line ->
        val (left, right) = line.split("-")
        Pair(left, right)
    }
    return UndirectedGraph.fromAssociations(associations)
}

// This solution has suboptimal complexity but performs fine on the input and doesn't involve much mutable state.
private fun part1(graph: UndirectedGraph<String>): Int {
    val tVertices = graph.vertices.filter { vertex -> vertex.name.first() == 't' }
    val visited = tVertices.associateWith { false }.toMutableMap().withDefault { false }
    return tVertices.sumOf { vertex1 ->
        visited[vertex1] = true
        val unvisitedNeighbors = vertex1.neighbors.filter { vertex -> !visited.getValue(vertex) }
        unvisitedNeighbors.distinctUnorderedPairs().count { (vertex2, vertex3) ->
            vertex2 in vertex3.neighbors
        }
    }
}
