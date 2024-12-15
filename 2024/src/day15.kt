import java.io.File

fun main() = solve("15", ::parse, ::part1, ::part2)

private data class Input15(val atlas: Matrix<Char>, val startingPos: Vec2, val movements: List<Vec2.CardinalDirection>)

private fun parse(file: File): Input15 {
    val lines = file.readLines()
    val atlasLines = lines.takeWhile(String::isNotBlank)
    var startingPos: Vec2? = null
    val atlas = Matrix(atlasLines) { pos, c ->
        if (c == '@') {
            startingPos = pos
            '.'
        } else c
    }
    requireNotNull(startingPos)
    val movementsLines = lines.takeLastWhile(String::isNotBlank)
    val movements = movementsLines.joinToString("").map { char ->
        when (char) {
            '^' -> Vec2.CardinalDirection.NORTH
            '>' -> Vec2.CardinalDirection.EAST
            'v' -> Vec2.CardinalDirection.SOUTH
            '<' -> Vec2.CardinalDirection.WEST
            else -> throw IllegalArgumentException("invalid character '$char' in list of movements")
        }
    }
    return Input15(atlas, startingPos!!, movements)
}

private fun part1(input: Input15): Int {
    val atlas = input.atlas.toMutable()
    var robotPos = input.startingPos
    for (dir in input.movements) {
        robotPos = moveRobot(robotPos, dir, atlas)
    }
    return gpsCoordinateSum(atlas)
}

// Attempt to move the robot in the given direction, potentially modifying atlas. Returns the new position of the robot.
private fun moveRobot(currPos: Vec2, dir: Vec2.CardinalDirection, atlas: MutableMatrix<Char>): Vec2 {
    val moveTo = currPos + dir
    if (atlas[moveTo] == '.') return moveTo
    val pushTo = findBoxDestination(moveTo, dir, atlas) ?: return currPos
    atlas[moveTo] = '.'
    atlas[pushTo] = 'O'
    return moveTo
}

// Find the first empty space after the line of boxes, or return null if the boxes are blocked by a wall.
private fun findBoxDestination(startPos: Vec2, dir: Vec2.CardinalDirection, atlas: Matrix<Char>): Vec2? {
    var currPos = startPos
    while (true) {
        when (atlas[currPos]) {
            '.' -> return currPos
            '#' -> return null
        }
        currPos += dir
    }
}

private fun gpsCoordinateSum(atlas: Matrix<Char>): Int = atlas.toSequence().sumOf { (pos, char) ->
    if (char == 'O' || char == '[') 100 * pos.y + pos.x
    else 0
}

private fun part2(input: Input15): Int {
    val atlas = widenAtlas(input.atlas).toMutable()
    var robotPos = Vec2(input.startingPos.x * 2, input.startingPos.y)
    for (dir in input.movements) {
        robotPos = when (dir) {
            Vec2.CardinalDirection.EAST,  Vec2.CardinalDirection.WEST  -> ::moveHorizontally
            Vec2.CardinalDirection.NORTH, Vec2.CardinalDirection.SOUTH -> ::moveVertically
        }(robotPos, dir, atlas)
    }
    return gpsCoordinateSum(atlas)
}

private fun widenAtlas(atlas: Matrix<Char>): Matrix<Char> {
    val newSize = Vec2(atlas.size.x * 2, atlas.size.y)
    return Matrix(newSize) { pos ->
        val sourceChar = atlas[pos.x / 2, pos.y]
        val isLeftSide = pos.x % 2 == 0
        when (sourceChar) {
            '#' -> '#'
            '.' -> '.'
            'O' -> if (isLeftSide) '[' else ']'
            '@' -> if (isLeftSide) '@' else '.'
            else -> throw IllegalArgumentException("invalid character $sourceChar in atlas")
        }
    }
}

// Like moveRobot, for east/west movement on the wide grid.
private fun moveHorizontally(currPos: Vec2, dir: Vec2.CardinalDirection, atlas: MutableMatrix<Char>): Vec2 {
    val moveTo = currPos + dir
    var pushTo = findBoxDestination(moveTo, dir, atlas) ?: return currPos
    // iterate through positions from the end backwards
    while (pushTo != currPos) {
        val nextPushTo = pushTo - dir
        atlas[pushTo] = atlas[nextPushTo]
        pushTo = nextPushTo
    }
    return moveTo
}

// Like moveRobot, for north/south movement on the wide grid.
private fun moveVertically(currPos: Vec2, dir: Vec2.CardinalDirection, atlas: MutableMatrix<Char>): Vec2 {
    // given a list of positions in the current row that are being pushed into by an object on the previous row,
    // determine if the push can happen (this is the return value). if so, carry it out.
    fun recursive(pushedIntoPoses: List<Vec2>): Boolean {
        // pushedIntoPoses is the set of positions in this row that are being pushed into by an object in the previous
        // row
        if (pushedIntoPoses.isEmpty()) return true
        val isBlocked = pushedIntoPoses.any { pos -> atlas[pos] == '#' }
        if (isBlocked) return false
        // the set of positions in the next row that are being pushed into by an object in this row
        val pushingIntoPoses = wideBoxPosesIn(pushedIntoPoses, atlas).map { pos -> pos + dir }
        if (!recursive(pushingIntoPoses)) return false
        // otherwise the push can happen, and the deeper recursive calls have already moved the boxes in the next row,
        // so move the boxes in the current row
        for (pushingIntoPos in pushingIntoPoses) {
            val pushingFromPos = pushingIntoPos - dir
            atlas[pushingIntoPos] = atlas[pushingFromPos]
            atlas[pushingFromPos] = '.'
        }
        return true
    }

    val moveTo = currPos + dir
    val pushed = recursive(listOf(moveTo))
    return if (pushed) moveTo else currPos
}

// Given a set of positions, find all the positions occupied by wide boxes who have at least one side in the set.
private fun wideBoxPosesIn(checkPoses: Collection<Vec2>, atlas: Matrix<Char>): Set<Vec2> {
    val boxPoses = mutableSetOf<Vec2>()
    for (checkPos in checkPoses) {
        when (atlas[checkPos]) {
            '[' -> {
                boxPoses.add(checkPos)
                boxPoses.add(checkPos + Vec2.CardinalDirection.EAST)
            }
            ']' -> {
                boxPoses.add(checkPos)
                boxPoses.add(checkPos + Vec2.CardinalDirection.WEST)
            }
        }
    }
    return boxPoses
}

// Testing function to print the current state of the boxes and robot.
private fun printState(atlas: Matrix<Char>, robotPos: Vec2) {
    if (atlas[robotPos] != '.') println("illegal character ${atlas[robotPos]} at robot position")
    println(atlas.toMutable().mapIndexed { pos, char ->
        if (pos == robotPos) '@' else char
    })
}