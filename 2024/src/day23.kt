import java.io.File

fun main() = solve("23", ::parse, ::part1, ::part2)

private fun parse(file: File): UndirectedGraph<String> {
    val associations = file.readLines().map { line ->
        val (left, right) = line.split("-")
        Pair(left, right)
    }
    return UndirectedGraph.fromAssociations(associations)
}

// This solution has suboptimal complexity, but it performs fine on the input and doesn't involve much mutable state.
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

private fun part2(graph: UndirectedGraph<String>): String {
    return maxClique(graph).map { vertex -> vertex.name }.sorted().joinToString(",")
}

// Find the maximum clique in the graph. Taken from algorithm 1 in "A review on algorithms for maximum clique problems"
// by Qinghua Wu & Jin-Kao Hao (p. 8), itself taken from "An exact algorithm for the maximum clique problem" by
// Carraghan & Pardalos.
// https://univ-angers.hal.science/hal-02709508/document
private fun <E> maxClique(graph: UndirectedGraph<E>): Set<UndirectedGraph.Vertex<E>> {
    var maxClique = setOf<UndirectedGraph.Vertex<E>>()
    fun search(
        // the clique we're building up
        currClique: Set<UndirectedGraph.Vertex<E>>,
        // the (ordered) set of vertices that are adjacent to all the vertices in the clique
        extensions: List<UndirectedGraph.Vertex<E>>
    ) {
        if (currClique.size > maxClique.size) maxClique = currClique
        if (currClique.size + extensions.size <= maxClique.size) return

        // choose each vertex to add to the clique and recurse on the resulting clique
        for ((extension, restExtensions) in extensions.poppingIterator()) {
            val nextClique = currClique + extension
            val nextExtensions = (restExtensions intersect extension.neighbors).toList()
            search(nextClique, nextExtensions)
        }
    }
    search(setOf(), graph.vertices.sortedBy { vertex -> vertex.neighbors.size })
    return maxClique
}

// Iterate through the immutable list as if it were a queue. At each iteration it yields the next element and the
// sublist that comes after that element.
private fun <E> List<E>.poppingIterator(): Iterator<Pair<E, List<E>>> = object: Iterator<Pair<E, List<E>>> {
    var currList = this@poppingIterator
    override fun hasNext(): Boolean = currList.isNotEmpty()
    override fun next(): Pair<E, List<E>> {
        val el = currList.first()
        currList = currList.dropFirstView()
        return Pair(el, currList)
    }
}
