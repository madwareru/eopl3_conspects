package exercises

data class EnvScope<T, TKey, TVal> (
    val emptyEnv: () -> T,
    val isEmpty: (T) -> Boolean,
    val extendEnv: (T, TKey, TVal) -> T,
    val extendEnvMany: (T, Array<Pair<TKey, TVal>>) -> T,
    val hasBinding: (T, TKey) -> Boolean,
    val applyEnv: (T, TKey, (TVal) -> Unit) -> Unit
)

sealed class PairEnvExp<TKey, TVal> {
    class Empty<TKey, TVal> : PairEnvExp<TKey, TVal>()
    data class Extend<TKey, TVal>(
        val binding: Pair<TKey, TVal>,
        val rest: PairEnvExp<TKey, TVal>
    ) : PairEnvExp<TKey, TVal>()
}

fun <TKey, TValue, TOut> PairEnvExp<TKey, TValue>.applyImpl(k: TKey, action: (TValue) -> TOut): TOut {
    return when (this) {
        is PairEnvExp.Empty -> throw NoSuchElementException("the binding for the name $k not found!")
        is PairEnvExp.Extend -> {
            val (name, value) = binding
            if (name == k) {
                action(value)
            } else {
                rest.applyImpl(k, action)
            }
        }
    }
}

fun <TKey, TValue> PairEnvExp<TKey, TValue>.hasBindingImpl(k: TKey): Boolean {
    return when (this) {
        is PairEnvExp.Empty -> false
        is PairEnvExp.Extend -> { binding.first == k || rest.hasBindingImpl(k) }
    }
}

val pairEnvScope = EnvScope<PairEnvExp<String, Int>, String, Int>(
    emptyEnv = { PairEnvExp.Empty() },
    isEmpty = { it is PairEnvExp.Empty },
    extendEnv = { env, k, v -> PairEnvExp.Extend( binding = k to v, rest = env ) },
    extendEnvMany = { env, pairs ->
        pairs
            .reversed()
            .fold(env) { e, binding -> PairEnvExp.Extend( binding, rest = e ) }
    },
    hasBinding = { env, k -> env.hasBindingImpl(k) },
    applyEnv = { env, k, action -> env.applyImpl(k, action) }
)

sealed class RibCageEnvExp<TKey, TVal> {
    class Empty<TKey, TVal> : RibCageEnvExp<TKey, TVal>()
    data class Extend<TKey, TVal>(
        val bindings: Array<Pair<TKey, TVal>>,
        val rest: RibCageEnvExp<TKey, TVal>
    ) : RibCageEnvExp<TKey, TVal>()
}

fun <TKey, TValue, TOut> RibCageEnvExp<TKey, TValue>.applyImpl(k: TKey, action: (TValue) -> TOut): TOut {
    return when (this) {
        is RibCageEnvExp.Empty -> throw NoSuchElementException("the binding for the name $k not found!")
        is RibCageEnvExp.Extend -> {
            bindings
                .firstOrNull { it.first == k }
                ?.let { action(it.second) }
                ?: rest.applyImpl(k, action)
        }
    }
}

fun <TKey, TValue> RibCageEnvExp<TKey, TValue>.hasBindingImpl(k: TKey): Boolean =
    when (this) {
        is RibCageEnvExp.Empty -> false
        is RibCageEnvExp.Extend -> {
            bindings.firstOrNull { it.first == k } != null
        }
    }

val ribCageEnvScope = EnvScope<RibCageEnvExp<String, Int>, String, Int>(
    emptyEnv = { RibCageEnvExp.Empty() },
    isEmpty = { it is RibCageEnvExp.Empty },
    extendEnv = { env, k, v -> RibCageEnvExp.Extend( bindings = arrayOf(k to v), rest = env ) },
    extendEnvMany = { env, pairs -> RibCageEnvExp.Extend( bindings = pairs, rest = env )},
    hasBinding = { env, k -> env.hasBindingImpl(k) },
    applyEnv = { env, k, action -> env.applyImpl(k, action) }
)

fun <TKey, TVal> makeEmptyEnv(): ((TKey) -> TVal?)? = null
fun <TKey, TVal> ((TKey) -> TVal?)?.extendEnv(savedVar: TKey, savedVal: TVal): (TKey) -> TVal? =
    { searchVar -> if (searchVar == savedVar) { savedVal } else { this?.let { this(searchVar) } } }

val proceduralEnvScope = EnvScope<((String) -> Int?)?, String, Int>(
    emptyEnv = { makeEmptyEnv() },
    isEmpty = { it == null },
    extendEnv = { env, k, v -> env.extendEnv(k, v) },
    extendEnvMany = { env, pairs ->
        pairs
            .reversed()
            .fold(env) { e, binding -> e.extendEnv(binding.first, binding.second)  }
    },
    hasBinding = { env, k -> env != null && env(k) != null },
    applyEnv = { env, k, action ->
        env?.let { it(k) }
            ?.let(action)
            ?: throw NoSuchElementException("the binding for the name $k not found!")
    }
)

fun ex2_5() {
    fun<T> testScope(scope: () -> EnvScope<T, String, Int>): () -> Unit = {
        with(scope()) {
            val env = extendEnvMany(emptyEnv(), arrayOf("a" to 10, "b" to 20, "c" to 30))

            applyEnv(env, "a") {v -> println("binding for a in env is $v") }
            applyEnv(env, "b") {v -> println("binding for b in env is $v") }
            applyEnv(env, "c") {v -> println("binding for c in env is $v") }
            println("binding for a exists? ${hasBinding(env, "a")}")
            println("binding for b exists? ${hasBinding(env, "b")}")
            println("binding for c exists? ${hasBinding(env, "c")}")
            println("binding for d exists? ${hasBinding(env, "d")}")

            val env2 = extendEnvMany(env, arrayOf( "b" to 100, "a" to 1000) )

            applyEnv(env2, "a") {v -> println("binding for a in env2 is $v") }
            applyEnv(env2, "b") {v -> println("binding for b in env2 is $v") }
            applyEnv(env2, "c") {v -> println("binding for c in env2 is $v") }
        }
    }

    arrayOf(
        "pair repr" to testScope { pairEnvScope },
        "rib cage repr" to testScope { ribCageEnvScope },
        "procedural repr" to testScope { proceduralEnvScope }
    ).forEach {
        val (label, testScope) = it
        println("testing $label:")
        testScope()
        println()
    }
}