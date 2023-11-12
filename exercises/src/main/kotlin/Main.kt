import exercises.ex2_1
import exercises.ex2_15
import exercises.ex2_5
import java.io.BufferedReader
import java.io.InputStreamReader

val exerciseLookup = hashMapOf(
    "2.1" to ::ex2_1,
    "2.5" to ::ex2_5,
    "2.15" to ::ex2_15
)

fun main() {
    val input = InputStreamReader(System.`in`)
    val reader = BufferedReader(input)

    println("please insert exercise name:")
    val name = reader.readLine()
    exerciseLookup[name]?.invoke() ?: println("ex $name not found!")
}