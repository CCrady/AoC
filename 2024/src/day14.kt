import java.io.File

fun main() {
    var input: Pair<Vec2, List<Robot>>? = null
    solve("14", { file ->
        parse(file).also { parsed -> input = parsed }
    }, ::part1)
    part2(input!!)
}

// In order to make the width and height available to the code, add a line at the start of the input file of the form
// <width>x<height>, e.g. "101x103".

private data class Robot(val pos: Vec2, val vel: Vec2)

private fun parse(file: File): Pair<Vec2, List<Robot>> {
    val lines = file.readLines()
    val (width, height) = """(\d+)x(\d+)""".toRegex().matchToInts(lines.first())
    val robots = lines.drop(1).map { line ->
        val (posX, posY, velX, velY) = """p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)""".toRegex().matchToInts(line)
        Robot(Vec2(posX, posY), Vec2(velX, velY))
    }
    return Pair(Vec2(width, height), robots)
}

private fun part1(input: Pair<Vec2, List<Robot>>): Int {
    val (bounds, robotsStart) = input
    val robotsEndPoses = robotsStart.map { (pos, vel) ->
        (pos + vel * 100) % bounds
    }
    val numRobotsInQuadrants: List<Int> = robotsEndPoses.fold(mutableListOf(0, 0, 0, 0)) { list, pos ->
        val quadrant = (pos - bounds / 2).sign
        when (quadrant) {
            Vec2(-1, -1) -> { list[0]++ }
            Vec2(1,  -1) -> { list[1]++ }
            Vec2(1,  1 ) -> { list[2]++ }
            Vec2(-1, 1 ) -> { list[3]++ }
        }
        list
    }
    return numRobotsInQuadrants.fold(1, Int::times)
}

// Determining whether the robot swarm is forming a picture of a Christmas tree requires a subjective judgement, so we
// display likely candidates to the user until they find a suitable state.
private fun part2(input: Pair<Vec2, List<Robot>>) {
    println("part 2 real (press enter to search):")
    val (bounds, robots) = input
    // arrived at value of .3 by experimentation. this worked for my input, but it might not for yours.
    val deviationThreshold = (.3 * bounds.y).toInt()
    val robotVels = robots.map { robot -> robot.vel }
    val currRobotPoses = robots.map { robot -> robot.pos }.toMutableList()
    var currIteration = 0
    while (readln().isBlank()) {
        do {
            stepRobots(bounds, robotVels, currRobotPoses)
            currIteration++
        } while (averageDeviation(currRobotPoses) > deviationThreshold)
        printRobots(bounds, currRobotPoses)
        println(currIteration)
    }
}

private fun stepRobots(bounds: Vec2, robotVels: List<Vec2>, currRobotPoses: MutableList<Vec2>) {
    for (i in currRobotPoses.indices) {
        currRobotPoses[i] = (currRobotPoses[i] + robotVels[i]) % bounds
    }
}

// The average manhattan distance from a robot to the centroid of the swarm.
private fun averageDeviation(robotPoses: List<Vec2>): Int {
    val centroid = robotPoses.fold(Vec2.ZERO, Vec2::plus) / robotPoses.size
    return robotPoses.sumOf { pos -> (pos - centroid).mag } / robotPoses.size
}

private fun printRobots(bounds: Vec2, robotPoses: List<Vec2>) {
    val grid = MutableMatrix(bounds) { ' ' }
    for (robotPos in robotPoses) {
        grid[robotPos] = 'X'
    }
    println(grid)
}