package exercises

sealed class Option<out T> {
    data class Some<out T>(val value: T) : Option<T>()
    class None<out T>: Option<T>()
}

val<T> Option<T>.isNone get() = this is Option.None<T>
inline fun<T0, T1> Option<T0>.map(mapper: (T0) -> T1): Option<T1> =
    when (this) {
        is Option.None -> none()
        is Option.Some -> some { mapper(this.value) }
    }

inline fun<T0, T1> Option<T0>.flatMap(mapper: (T0) -> Option<T1>) =
    when (this) {
        is Option.None -> none()
        is Option.Some -> mapper(this.value)
    }

inline fun<T> Option<T>.unwrapOr(default: () -> T) =
    when (this) {
        is Option.Some -> this.value
        else -> default()
    }

fun <T> Option<T>.unwrap() = (this as Option.Some).value

inline fun <reified T> Any.tryCast(): Option<T> =
    if (this is T) { some { this } } else { none() }

inline fun<T> some(x: () -> T) = Option.Some(x())
fun<T> none() = Option.None<T>()

inline fun <T> Array<out T>.firstOrNone(predicate: (T) -> Boolean): Option<T> {
    for (element in this) if (predicate(element)) return some { element }
    return exercises.none()
}