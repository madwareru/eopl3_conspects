package exercises

interface EnvironmentADT<TImpl, TKey, TVal> {
    fun empty(): TImpl
    fun TImpl.isEmpty(): Boolean
    fun TImpl.extend(key: TKey, value: TVal): TImpl
    fun TImpl.extendMany(pairs: Array<Pair<TKey, TVal>>): TImpl
    fun TImpl.hasBinding(key: TKey): Boolean
    fun <TOut> TImpl.apply(key: TKey, action: (TVal) -> TOut): TOut
}

val pairEnvADT = object : EnvironmentADT<PairEnvExp<String, Int>, String, Int> {
    override fun empty(): PairEnvExp<String, Int> = PairEnvExp.Empty()

    override fun <TOut> PairEnvExp<String, Int>.apply(key: String, action: (Int) -> TOut): TOut = applyImpl(key, action)

    override fun PairEnvExp<String, Int>.hasBinding(key: String): Boolean = hasBindingImpl(key)

    override fun PairEnvExp<String, Int>.extendMany(pairs: Array<Pair<String, Int>>): PairEnvExp<String, Int> =
        pairs.reversed().fold(this) { e, binding -> PairEnvExp.Extend( binding, rest = e ) }

    override fun PairEnvExp<String, Int>.extend(key: String, value: Int): PairEnvExp<String, Int> =
        PairEnvExp.Extend( binding = key to value, rest = this )

    override fun PairEnvExp<String, Int>.isEmpty(): Boolean = this is PairEnvExp.Empty
}

val ribCageEnvADT = object : EnvironmentADT<RibCageEnvExp<String, Int>, String, Int> {
    override fun empty(): RibCageEnvExp<String, Int> = RibCageEnvExp.Empty()
    override fun <TOut> RibCageEnvExp<String, Int>.apply(key: String, action: (Int) -> TOut): TOut = applyImpl(key, action)

    override fun RibCageEnvExp<String, Int>.hasBinding(key: String): Boolean = hasBindingImpl(key)

    override fun RibCageEnvExp<String, Int>.extendMany(pairs: Array<Pair<String, Int>>): RibCageEnvExp<String, Int> =
        RibCageEnvExp.Extend(pairs, this)

    override fun RibCageEnvExp<String, Int>.extend(key: String, value: Int): RibCageEnvExp<String, Int> =
        RibCageEnvExp.Extend( arrayOf(key to value), rest = this )

    override fun RibCageEnvExp<String, Int>.isEmpty(): Boolean = this is RibCageEnvExp.Empty
}

val proceduralEnvADT = object : EnvironmentADT<((String) -> Int?)?, String, Int> {
    override fun empty(): ((String) -> Int?)? = makeEmptyEnv()

    override fun <TOut> ((String) -> Int?)?.apply(key: String, action: (Int) -> TOut): TOut {
        return this
            ?.let { it(key) }
            ?.let(action)
            ?: throw NoSuchElementException("the binding for the name $key not found!")
    }

    override fun ((String) -> Int?)?.hasBinding(key: String): Boolean =
        this != null && this(key) != null

    override fun ((String) -> Int?)?.extendMany(pairs: Array<Pair<String, Int>>): ((String) -> Int?)? {
        return pairs
            .reversed()
            .fold(this) { e, binding -> e.extendEnv(binding.first, binding.second)  }
    }

    override fun ((String) -> Int?)?.extend(key: String, value: Int): (String) -> Int? = extendEnv(key, value)

    override fun ((String) -> Int?)?.isEmpty(): Boolean = this == null
}

fun ex2_5_v2() {
    fun<T> testScope(scope: () -> EnvironmentADT<T, String, Int>): () -> Unit = {
        with(scope()) {
            val env = empty().extendMany(arrayOf("a" to 10, "b" to 20, "c" to 30))

            env.apply("a") { v -> println("binding for a in env is $v") }
            env.apply("b") { v -> println("binding for b in env is $v") }
            env.apply("c") { v -> println("binding for c in env is $v") }

            println("binding for a exists? ${env.hasBinding("a")}")
            println("binding for b exists? ${env.hasBinding("b")}")
            println("binding for c exists? ${env.hasBinding("c")}")
            println("binding for d exists? ${env.hasBinding("d")}")

            val env2 = env.extendMany(arrayOf( "b" to 100, "a" to 1000) )

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