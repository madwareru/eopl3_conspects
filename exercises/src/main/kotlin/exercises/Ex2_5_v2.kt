package exercises

interface EnvironmentADT<TImpl, TKey, TVal> {
    fun empty(): TImpl
    fun TImpl.isEmpty(): Boolean
    fun TImpl.extend(key: TKey, value: TVal): TImpl
    fun TImpl.extendMany(vararg pairs: Pair<TKey, TVal>): TImpl
    fun TImpl.hasBinding(key: TKey): Boolean
    fun <TOut> TImpl.apply(key: TKey, action: (TVal) -> TOut): TOut
}

val pairEnvADT = object : EnvironmentADT<PairEnvExp<String, Int>, String, Int> {
    override fun empty(): PairEnvExp<String, Int> = PairEnvExp.Empty()

    override fun <TOut> PairEnvExp<String, Int>.apply(key: String, action: (Int) -> TOut): TOut =
        applyImpl(key, action)

    override fun PairEnvExp<String, Int>.hasBinding(key: String): Boolean = hasBindingImpl(key)

    override fun PairEnvExp<String, Int>.extendMany(vararg pairs: Pair<String, Int>): PairEnvExp<String, Int> =
        pairs.reversed().fold(this) { e, binding -> PairEnvExp.Extend( binding, rest = e ) }

    override fun PairEnvExp<String, Int>.extend(key: String, value: Int): PairEnvExp<String, Int> =
        PairEnvExp.Extend( binding = key to value, rest = this )

    override fun PairEnvExp<String, Int>.isEmpty(): Boolean = this is PairEnvExp.Empty
}

val ribCageEnvADT = object : EnvironmentADT<RibCageEnvExp<String, Int>, String, Int> {
    override fun empty(): RibCageEnvExp<String, Int> = RibCageEnvExp.Empty()
    override fun <TOut> RibCageEnvExp<String, Int>.apply(key: String, action: (Int) -> TOut): TOut =
        applyImpl(key, action)

    override fun RibCageEnvExp<String, Int>.hasBinding(key: String): Boolean = hasBindingImpl(key)

    override fun RibCageEnvExp<String, Int>.extendMany(vararg pairs: Pair<String, Int>): RibCageEnvExp<String, Int> =
        RibCageEnvExp.Extend(pairs.map { it }.toTypedArray(), this)

    override fun RibCageEnvExp<String, Int>.extend(key: String, value: Int): RibCageEnvExp<String, Int> =
        RibCageEnvExp.Extend( arrayOf(key to value), rest = this )

    override fun RibCageEnvExp<String, Int>.isEmpty(): Boolean = this is RibCageEnvExp.Empty
}

val proceduralEnvADT = object : EnvironmentADT<Option<(String) -> Option<Int>>, String, Int> {
    override fun empty(): Option<(String) -> Option<Int>> = makeEmptyEnv()

    override fun <TOut> Option<(String) -> Option<Int>>.apply(key: String, action: (Int) -> TOut): TOut =
        flatMap { it(key) }
            .map (action)
            .unwrapOr {
                throw NoSuchElementException("the binding for the name $key not found!")
            }

    override fun Option<(String) -> Option<Int>>.hasBinding(key: String): Boolean =
        map { it(key) } is Option.Some

    override fun Option<(String) -> Option<Int>>.extendMany(
        vararg pairs: Pair<String, Int>): Option<(String
    ) -> Option<Int>> {
        return pairs
            .reversed()
            .fold(this) { e, binding -> some {e.extendEnv(binding.first, binding.second) } }
    }

    override fun Option<(String) -> Option<Int>>.extend(key: String, value: Int): Option<(String) -> Option<Int>> =
        some { extendEnv(key, value) }

    override fun Option<(String) -> Option<Int>>.isEmpty(): Boolean = this is Option.None
}

fun ex2_5_v2() {
    fun<T> testScope(scope: () -> EnvironmentADT<T, String, Int>): () -> Unit = {
        with(scope()) {
            val env = empty().extendMany("a" to 10, "b" to 20, "c" to 30)

            env.apply("a") { v -> println("binding for a in env is $v") }
            env.apply("b") { v -> println("binding for b in env is $v") }
            env.apply("c") { v -> println("binding for c in env is $v") }

            println("binding for a exists? ${env.hasBinding("a")}")
            println("binding for b exists? ${env.hasBinding("b")}")
            println("binding for c exists? ${env.hasBinding("c")}")
            println("binding for d exists? ${env.hasBinding("d")}")

            val env2 = env.extendMany( "b" to 100, "a" to 1000)

            env2.apply("a") { v -> println("binding for a in env2 is $v") }
            env2.apply("b") { v -> println("binding for b in env2 is $v") }
            env2.apply("c") { v -> println("binding for c in env2 is $v") }
        }
    }

    arrayOf(
        "pair repr" to testScope { pairEnvADT },
        "rib cage repr" to testScope { ribCageEnvADT },
        "procedural repr" to testScope { proceduralEnvADT }
    ).forEach {
        val (label, testScope) = it
        println("testing $label:")
        testScope()
        println()
    }
}