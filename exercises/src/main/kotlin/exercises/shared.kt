package exercises

sealed class Option<out T> {
    data class Some<out T>(val value: T) : Option<T>()
    class None<out T>: Option<T>()
}

inline fun<T0, T1> Option<T0>.flatMap(mapper: (T0) -> Option<T1>): Option<T1> =
    when (this) {
        is Option.None -> none()
        is Option.Some -> mapper(this.value)
    }

inline fun<T0, T1> Option<T0>.map(mapper: (T0) -> T1): Option<T1> =
    flatMap { some { mapper(it) } }

inline fun<T> Option<T>.unwrapOr(default: () -> T) =
    when (this) {
        is Option.Some -> this.value
        else -> default()
    }

inline fun<T> Option<T>.or(alternative: () -> Option<T>): Option<T> = when (this) {
    is Option.Some -> this
    else -> alternative()
}

inline fun<T, TErrorCause> Option<T>.okOr(alternative: () -> Result<T, TErrorCause>): Result<T, TErrorCause> =
    map { ok<T, TErrorCause> { it } }
        .unwrapOr { alternative() }

inline fun <reified T> Any.tryCast(): Option<T> =
    if (this is T) { some { this } } else { none() }

inline fun<T> some(x: () -> T) = Option.Some(x())
fun<T> none() = Option.None<T>()

inline fun<T> Boolean.guard(action: () -> T): Option<T> =
    if (this) { some { action() } } else { none() }

inline fun <T> Array<out T>.firstOrNone(predicate: (T) -> Boolean): Option<T> {
    for (element in this) if (predicate(element)) return some { element }
    return exercises.none()
}

inline fun <T> List<T>.firstOrNone(predicate: (T) -> Boolean): Option<T> {
    for (element in this) if (predicate(element)) return some { element }
    return exercises.none()
}

sealed class Result<out T, out TErrorReason> {
    data class Ok<out T, out TErrorReason>(val value: T) : Result<T, TErrorReason>()
    data class Err<out T, out TErrorReason>(val reason: TErrorReason) : Result<T, TErrorReason>()
}

inline fun<T, TErrorReason> ok(x: () -> T) = Result.Ok<T, TErrorReason>(x())
inline fun<T, TErrorReason> err(x: () -> TErrorReason) = Result.Err<T, TErrorReason>(x())

inline fun<T0, T1, TErrorReason>
Result<T0, TErrorReason>.flatMap(mapper: (T0) -> Result<T1, TErrorReason>): Result<T1, TErrorReason> =
    when (this) {
        is Result.Err -> err { this.reason }
        is Result.Ok -> mapper(this.value)
    }

inline fun<T0, T1, TErrorReason>
Result<T0, TErrorReason>.map(mapper: (T0) -> T1): Result<T1, TErrorReason> =
    flatMap { ok { mapper(it) } }