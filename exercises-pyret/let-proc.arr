type LetProcEnv = List<{String; LetProcValue}>
type LetProcResult = Result<LetProcValue, LetProcError>

data ControlFlow<a, b>: next(v :: a) | brk(v :: b) end
fun loop<a, b>(func :: (a -> ControlFlow<a, b>), init :: a) -> b:
  cases(ControlFlow) func(init):
    | brk(v) => v
    | next(v) => loop(func, v)
  end
end

fun env-get(env :: LetProcEnv, name :: String) -> LetProcResult:
  env
    .find({({n; _}): n == name})
    .and-then({({_; v}): ok(v)})
    .or-else(err(lpe-name-not-found(name)))
end

fun flat-map(mapping, res):
  res.flat-map(mapping)
end

fun fmap(mapping, res):
  res.map(mapping)
end

data Result<T, E>:
  | ok(r :: T)
  | err(e :: E)
sharing:
  method map<T1>(self, mapping :: (T -> T1)) -> Result<T1, E>:
    cases(Result) self:
      | ok(r) => ok(mapping(r))
      | err(e) => err(e)
    end
  end,
  method flat-map<T1>(self, mapping :: (T -> Result<T1, E>)) -> Result<T1, E>:
    cases(Result) self:
      | ok(r) => mapping(r)
      | err(e) => err(e)
    end
  end
end

data Parser<TInput, T, E>:
    parser(lambda :: (TInput -> Result<{T; TInput}, E>)) with:
    method try-parse(self, input :: TInput) -> Result<{T; TInput}, E>:
      self.lambda(input)
    end,
    method map<T1>(self, mapping :: (T -> T1)) -> Parser<TInput, T1, E>:
      parser(
        {(in):
          self.lambda(in).map({({x; xs}): {mapping(x); xs}})
        })
    end,
    method flat-map<T1>(self, mapping :: (T -> Parser<TInput, T1, E>)) -> Parser<TInput, T1, E>:
      parser(
        {(in):
          cases(Result) self.lambda(in):
            | ok({r; rest}) => mapping(r).lambda(rest)
            | err(e) => err(e)
          end
        })
    end,
    method or-else(self, alternative :: Parser<TInput, T, E>) -> Parser<TInput, T, E>:
      parser(
        {(in):
          cases(Result) self.lambda(in):
            | ok({r; rest}) => ok({r; rest})
            | err(_) => alternative.lambda(in)
          end
        })
    end,
    method many(self) -> Parser<TInput, List<T>, E>:
      parser(
        {(in):
          cases(Result) self.lambda(in):
            | err(e) => err(e)
            | ok({fst; rem}) =>
              r = for loop({results; remainder} from { [list:]; rem}):
                cases(Result) self.lambda(remainder):
                  | ok({x; xs}) => next({ link(x, results); xs })
                  | else => brk({ results; remainder })
                end
              end
              ok({[list: fst].append(r.{0}.reverse()); r.{1}})
          end
        })
    end,
    method optional(self) -> Parser<TInput, Option<T>, E>:
      parser(
        {(in):
          cases(Result) self.lambda(in):
            | ok({r; rest}) => ok({some(r); rest})
            | else => ok({none; in})
          end
        })
    end
end

fun parser-ref<TInput, T, E>(reference :: (-> Parser<TInput, T, E>)) -> Parser<TInput, T, E>:
  parser({(in): reference().try-parse(in) })
end

fun parse-nothing<TToken, E>() -> Parser<List<TToken>, Nothing, E>:
  parser({(in):ok({nothing; in})})
end

fun parse-eof<TToken, E>(err-callback :: (-> E)) -> Parser<List<TToken>, Nothing, E>:
  parser(
    {(in):
      if is-empty(in):
        ok({nothing; in})
      else:
        err(err-callback())
      end
    })
where:
  my-parser = parse-eof({(): "expected eof, but got non-empty list!"})
  my-parser.try-parse([list:]) is ok({nothing; [list:]})
  my-parser.try-parse([list: 1, 2, 3]) is err("expected eof, but got non-empty list!")
  my-parser.try-parse([list: "a", "b", "c"]) is err("expected eof, but got non-empty list!")
end

fun parse-any-token<TToken, E>(err-callback :: (-> E)) -> Parser<List<TToken>, TToken, E>:
  parser(
    {(in):
      cases(List) in:
        | link(x, xs) => ok({x; xs})
        | empty => err(err-callback())
      end
    })
where:
  my-parser = parse-any-token({(): "expected any token, but got empty list!"})
  my-parser.try-parse(string-explode("abc")) is ok({"a"; [list: "b", "c"]})
  my-parser.try-parse(empty) is err("expected any token, but got empty list!")
end

fun parse-token<TToken, E>(
    predicate :: (TToken -> Boolean),
    err-callback :: (-> E)) -> Parser<List<TToken>, TToken, E>:
  parser(
    {(in):
      cases(Result) parse-any-token(err-callback).try-parse(in):
        | ok({x; rest}) =>
          if predicate(x):
            ok({x; rest})
          else:
            err(err-callback())
          end
        | err(e) => err(e)
      end
    })
where:
  my-digit-parser = parse-token(
    {(it): (it >= "0") and (it <= "9") },
    {(): "expected digit, but got something else!"}
    )
  my-hex-digit-part-parser = parse-token(
    {(it):
      code-point = string-to-code-point(it)
      code-point-a = string-to-code-point("a")
      code-point-f = string-to-code-point("f")
      code-point-A = string-to-code-point("A")
      code-point-F = string-to-code-point("F")
      ((code-point >= code-point-a) and (code-point <= code-point-f)) or
      ((code-point >= code-point-A) and (code-point <= code-point-F))
    },
    {(): "expected hex digit part, but got something else!"})
  my-hex-digit-parser = my-digit-parser.or-else(my-hex-digit-part-parser)

  my-digit-parser.try-parse(string-explode("123")) satisfies is-ok
  my-digit-parser.try-parse(empty) satisfies is-err
  my-digit-parser.try-parse(string-explode("abc123")) satisfies is-err

  my-hex-digit-parser.try-parse(string-explode("123")) satisfies is-ok
  my-hex-digit-parser.try-parse(empty) satisfies is-err
  my-hex-digit-parser.try-parse(string-explode("abc132")) satisfies is-ok
  my-hex-digit-parser.try-parse(string-explode("google")) satisfies is-err
end

fun parse-exact-token<TToken, E>(
    expected-token :: TToken,
    err-callback :: (-> E)) -> Parser<List<TToken>, TToken, E>:
  parse-token({(it): it == expected-token}, err-callback)
end

fun parse-char(c :: String) -> Parser<List<String>, String, String>:
  parse-exact-token(c, {(): "expected '" + c + "', but got something else!"})
end

fun parse-digit():
  parse-token(
    {(it): (it >= "0") and (it <= "9") },
    {(): "expected digit, but got something else!"})
end

fun parse-hex-digit():
  parse-token(
    {(it):
      ((it >= "0") and (it <= "9")) or
      ((it >= "a") and (it <= "f")) or
      ((it >= "A") and (it <= "F"))
    },
    {(): "expected hex digit, but got something else!"})
end

fun parse-letter():
  parse-token(
    {(it):
      ((it >= "a") and (it <= "z")) or
      ((it >= "A") and (it <= "Z"))
    },
    {(): "expected letter, but got something else!"})
end

fun parse-char-seq(chars :: List<String>) -> Parser<List<String>, Nothing, String>:
  cases(List) chars:
    | empty => parse-nothing()
    | link(x, xs) => parse-char(x).flat-map({(_): parse-char-seq(xs)})
  end
end

fun parse-string(s :: String) -> Parser<List<String>, String, String>:
  parse-char-seq(string-explode(s)).map({(_): s})
end

fun parse-whitespace() -> Parser<List<String>, Nothing, String>:
  parse-char(" ")
    .or-else(parse-char("\t"))
    .or-else(parse-char("\r"))
    .or-else(parse-char("\n"))
    .many()
    .map({(_): nothing})
where:
  my-parser =
    for flat-map(_ from parse-whitespace()):
      for flat-map(cap from parse-string("capibara")):
        for fmap(_ from parse-whitespace()):
          cap
        end
      end
    end

  my-parser.try-parse(string-explode("  capibara   abc"))
    is ok({"capibara"; string-explode("abc")})
end

fun tokenize<T, E>(p :: Parser<List<String>, T, E>):
  for flat-map(_ from parse-whitespace().optional()):
    for flat-map(x from p):
      for fmap(_ from parse-whitespace().optional()):
        x
      end
    end
  end
where:
  my-parser = tokenize(parse-string("capibara"))

  my-parser.try-parse(string-explode("  capibara   abc"))
    is ok({"capibara"; string-explode("abc")})

  my-parser.try-parse(string-explode("capibara   abc"))
    is ok({"capibara"; string-explode("abc")})

  my-parser.try-parse(string-explode("  capibaraabc"))
    is ok({"capibara"; string-explode("abc")})

  my-parser.try-parse(string-explode("capibaraabc"))
    is ok({"capibara"; string-explode("abc")})
end

check:
  umpalump-parser =
    for flat-map(_ from parse-char("u")):
      for flat-map(_ from parse-char("m")):
        for flat-map(_ from parse-char("p")):
          for flat-map(_ from parse-char("a")):
            for flat-map(_ from parse-char("l")):
              for flat-map(_ from parse-char("u")):
                for flat-map(_ from parse-char("m")):
                  for fmap(_ from parse-char("p")):
                    "umpalump"
                  end
                end
              end
            end
          end
        end
      end
    end

  umpalump-parser-2 = parse-char-seq(string-explode("umpalump"))

  parse-char("a").try-parse(string-explode("123")) satisfies is-err
  parse-char("a").try-parse(string-explode("abc")) satisfies is-ok

  many-a = parse-char("a").many()
  many-a.try-parse(string-explode("abc")) is ok({ [list: "a"]; [list: "b", "c"] })
  many-a.try-parse(string-explode("aaaabc")) is ok({ [list: "a", "a", "a", "a"]; [list: "b", "c"] })

  umpalump-parser.try-parse(string-explode("123")) satisfies is-err
  umpalump-parser.try-parse(string-explode("abc")) satisfies is-err
  umpalump-parser.try-parse(string-explode("umpalump")) satisfies is-ok
  umpalump-parser.try-parse(string-explode("umpalupm")) satisfies is-err
  umpalump-parser.try-parse(string-explode("umpalumpa")) satisfies is-ok

  umpalump-parser-2.try-parse(string-explode("123")) satisfies is-err
  umpalump-parser-2.try-parse(string-explode("abc")) satisfies is-err
  umpalump-parser-2.try-parse(string-explode("umpalump")) satisfies is-ok
  umpalump-parser-2.try-parse(string-explode("umpalupm")) satisfies is-err
  umpalump-parser-2.try-parse(string-explode("umpalumpa")) satisfies is-ok

  parse-string("umpalump").try-parse(string-explode("umpalumpa"))
    is ok({"umpalump"; [list: "a"]})
end

data LetProcError:
  | lpe-cant-compare-values
  | lpe-cant-cast-to-bool
  | lpe-cant-cast-to-number
  | lpe-cant-call-non-proc-value
  | lpe-cant-bind-recursive-to-non-proc
  | lpe-name-not-found(name :: String)
  | lpe-unknown
end

data LetProcValue:
  | lpv-num(n :: Number)
  | lpv-bool(b :: Boolean)
  | lpv-user-proc(u :: LetProcUser)
  | lpv-intrinsic-proc(i :: LetProcIntrinsic)
sharing:
  method as-bool(self) -> Result<Boolean, LetProcError>:
    cases(LetProcValue) self:
      | lpv-bool(b) => ok(b)
      | else => err(lpe-cant-cast-to-bool)
    end
  end,
  method as-number(self) -> Result<Number, LetProcError>:
    cases(LetProcValue) self:
      | lpv-num(n) => ok(n)
      | else => err(lpe-cant-cast-to-number)
    end
  end,
  method call(self, bindings :: List<LetProcValue>) -> LetProcResult:
    cases(LetProcValue) self:
      | lpv-user-proc(u) =>
        ask:
          | bindings.length() == u.arguments.length() then:
            new-env = for fold2(acc from u.env, name from u.arguments, value from bindings):
              acc.push({name; value})
            end

            new-env-ensured = cases(Option) u.recursive-name:
              | some(rec-name) => new-env.push({rec-name; self})
              | none => new-env
            end

            u.body.eval(new-env-ensured)
          | otherwise: ...
        end
      | lpv-intrinsic-proc(i) =>
        ask:
          | bindings.length() == i.arguments.length() then:
            new-env = for fold2(acc from i.env, name from i.arguments, value from bindings):
              acc.push({name; value})
            end
            i.body.eval(new-env)
          | otherwise: ...
        end
      | else => err(lpe-cant-call-non-proc-value)
    end
  end
end

data LetProcUser:
    user-proc(
      recursive-name :: Option<String>,
      arguments :: List<String>,
      env :: LetProcEnv,
      body :: LetProcExpression
    )
end

data LetProcIntrinsic:
    intrinsic-proc(
      arguments :: List<String>,
      env :: LetProcEnv,
      body :: Any
    )
end

fun lpv(x :: Any) -> LetProcValue:
  ask:
    | is-number(x) then: lpv-num(x)
    | is-boolean(x) then: lpv-bool(x)
    | is-LetProcUser(x) then: lpv-user-proc(x)
    | is-LetProcIntrinsic(x) then: lpv-intrinsic-proc(x)
    | otherwise: raise("invalid argument!")
  end
end

data LetProcExpression:
  | value-expr(v :: LetProcValue)
  | name-expr(name :: String)
  | lambda-expr(
      args :: List<String>,
      body :: LetProcExpression)
  | if-expr(
      cond :: LetProcExpression,
      thn :: LetProcExpression,
      els :: LetProcExpression)
  | let-expr(
      recursive :: Boolean,
      name :: String,
      value :: LetProcExpression,
      next-expr :: LetProcExpression)
  | call-expr(
      callee :: LetProcExpression,
      args :: List<LetProcExpression>)
sharing:
  method eval(self, env :: LetProcEnv) -> LetProcResult:
    cases(LetProcExpression) self:
      | value-expr(v) => ok(v)
      | name-expr(name) => env-get(env, name)
      | if-expr(cond, thn, els) =>
        cond.eval(env)
          .flat-map({(it): it.as-bool()})
          .flat-map({(cond-res):
            if cond-res: thn.eval(env) else: els.eval(env) end
          })
      | let-expr(recursive, name, value, next-expr) =>
        value.eval(env)
          .flat-map({(value-res):
            ask:
              | not(recursive) then: ok(value-res)
              | recursive and is-lpv-user-proc(value-res) then:
                ok(
                  lpv(
                    user-proc(
                      some(name),
                      value-res.u.arguments,
                      value-res.u.env,
                      value-res.u.body
                    )
                  )
                )
              | otherwise: err(lpe-cant-bind-recursive-to-non-proc)
            end
          }).flat-map({(v): next-expr.eval(env.push({name; v}))})
      | lambda-expr(args, body) => ok(lpv-user-proc(user-proc(none, args, env, body)))
      | call-expr(callee, args) =>
        partitioned = partition(is-err, args.map({(it): it.eval(env)}))
        {error-args; success-args} = {partitioned.is-true; partitioned.is-false}
        cases(List) error-args:
          | link(first-error, _) => first-error
          | else =>
            safe-args = success-args.map({(it): it.r})
            callee.eval(env).flat-map({(proc-res): proc-res.call(safe-args)})
        end
    end
  end
end

data CallSelf:
    call-self with:
    method into-lpv(self):
      lpv(
        intrinsic-proc(
          [list: "__proc__"],
          empty,
          self
        )
      )
    end,
    method eval(self, env :: LetProcEnv) -> LetProcResult:
      for flat-map(val from env-get(env, "__proc__")):
        ask:
          | is-lpv-user-proc(val) or is-lpv-intrinsic-proc(val) then:
            proc-expr = value-expr(val)
            call-expr(proc-expr, [list: proc-expr]).eval(env)
          | otherwise: err(lpe-cant-call-non-proc-value)
        end
      end
    end
end

data Op:
  | unary-op(v :: (LetProcValue -> LetProcResult)) with:
    method into-lpv(self):
      lpv(
        intrinsic-proc(
          [list: "__operand__"],
          empty,
          self
        )
      )
    end,
    method eval(self, env :: LetProcEnv) -> LetProcResult:
      for flat-map(operand from env-get(env, "__operand__")):
        self.v(operand)
      end
    end
  | binary-op(v :: ((l :: LetProcValue, r :: LetProcValue) -> LetProcResult)) with:
    method into-lpv(self):
      lpv(
        intrinsic-proc(
          [list: "__lhs__", "__rhs__"],
          empty,
          self
        )
      )
    end,
    method eval(self, env :: LetProcEnv) -> LetProcResult:
      for flat-map(lhs from env-get(env, "__lhs__")):
        for flat-map(rhs from env-get(env, "__rhs__")):
          self.v(lhs, rhs)
        end
      end
    end
end

fun add-bin-ops(
    env :: LetProcEnv,
    bin-ops :: List<{String; ((l :: LetProcValue, r :: LetProcValue) -> LetProcResult)}>
    ) -> LetProcEnv:
  for fold(e from env, {name; body} from bin-ops):
    e.push({name; binary-op(body).into-lpv()})
  end
end

fun add-unary-ops(
    env :: LetProcEnv,
    unary-ops :: List<{String; ((operand :: LetProcValue) -> LetProcResult)}>
    ) -> LetProcEnv:
  for fold(e from env, {name; body} from unary-ops):
    e.push({name; unary-op(body).into-lpv()})
  end
end

env-with-operators = empty
  ^ add-unary-ops(_,
  [list:
    {
      "identity";
      lam(op): ok(op) end
    },
    {
      "not";
      lam(op):
        for fmap(r from op.as-bool()):
          not(r)
        end
      end
    },
    {
      "minus";
      lam(op):
        for fmap(r from op.as-number()):
          r * -1
        end
      end
    },
    {
      "print";
      lam(op) block:
        print(op)
        ok(op)
      end
    },
    {
      "spy";
      lam(op) block:
        spy: op end
        ok(op)
      end
    }
  ])
  ^ add-bin-ops(_,
  [list:
    {
      "add";
      lam(l, r):
        for flat-map(lhs from l.as-number()):
          for fmap(rhs from r.as-number()):
            lpv(lhs + rhs)
          end
        end
      end
    },
    {
      "sub";
      lam(l, r):
        for flat-map(lhs from l.as-number()):
          for fmap(rhs from r.as-number()):
            lpv(lhs - rhs)
          end
        end
      end
    },
    {
      "mul";
      lam(l, r):
        for flat-map(lhs from l.as-number()):
          for fmap(rhs from r.as-number()):
            lpv(lhs * rhs)
          end
        end
      end
    },
    {
      "div";
      lam(l, r):
        for flat-map(lhs from l.as-number()):
          for fmap(rhs from r.as-number()):
            lpv(lhs / rhs)
          end
        end
      end
    },
    {
      "lt";
      lam(l, r):
        for flat-map(lhs from l.as-number()):
          for fmap(rhs from r.as-number()):
            lpv(lhs < rhs)
          end
        end
      end
    },
    {
      "lt-eq";
      lam(l, r):
        for flat-map(lhs from l.as-number()):
          for fmap(rhs from r.as-number()):
            lpv(lhs <= rhs)
          end
        end
      end
    },
    {
      "gt";
      lam(l, r):
        for flat-map(lhs from l.as-number()):
          for fmap(rhs from r.as-number()):
            lpv(lhs > rhs)
          end
        end
      end
    },
    {
      "gt-eq";
      lam(l, r):
        for flat-map(lhs from l.as-number()):
          for fmap(rhs from r.as-number()):
            lpv(lhs >= rhs)
          end
        end
      end
    },
    {
      "and";
      lam(l, r):
        for flat-map(lhs from l.as-bool()):
          for fmap(rhs from r.as-bool()):
            lpv(lhs and rhs)
          end
        end
      end
    },
    {
      "or";
      lam(l, r):
        for flat-map(lhs from l.as-bool()):
          for fmap(rhs from r.as-bool()):
            lpv(lhs or rhs)
          end
        end
      end
    },
    {
      "xor";
      lam(l, r):
        for flat-map(lhs from l.as-bool()):
          for fmap(rhs from r.as-bool()):
            lpv((lhs and not(rhs)) or (not(lhs) and rhs))
          end
        end
      end
    },
    {
      "eq";
      lam(l, r):
        cases(LetProcValue) l:
          | lpv-num(lhs) => for fmap(rhs from r.as-number()): lpv(lhs == rhs) end
          | lpv-bool(lhs) => for fmap(rhs from r.as-bool()): lpv(lhs == rhs) end
          | else => err(lpe-cant-compare-values)
        end
      end
    }
  ])

check:
  #setup:

  fun v-expr(x :: Any) -> LetProcExpression:
    value-expr(lpv(x))
  end

  empty-env = empty
  env-with-a = empty.push({"a"; lpv(10)})
  env-with-a-shadowed = env-with-a.push({"a"; lpv(2)})

  #tests:

  for each(env from [list: empty-env, env-with-a, env-with-a-shadowed]) block:
    v-expr(5).eval(env) is ok(lpv-num(5))
    v-expr(false).eval(env) is ok(lpv-bool(false))
  end

  name-expr("a").eval(empty-env) is err(lpe-name-not-found("a"))
  name-expr("a").eval(env-with-a) is ok(lpv(10))
  name-expr("a").eval(env-with-a-shadowed) is ok(lpv-num(2))
  name-expr("b").eval(env-with-a) is err(lpe-name-not-found("b"))
  name-expr("b").eval(env-with-a-shadowed) is err(lpe-name-not-found("b"))


  if-expr(v-expr(false), v-expr(1), v-expr(2)).eval(empty-env) is ok(lpv-num(2))
  if-expr(v-expr(true), v-expr(1), v-expr(2)).eval(empty-env) is ok(lpv-num(1))
  if-expr(v-expr(true), name-expr("a"), v-expr(2)).eval(env-with-a) is ok(lpv-num(10))

  let-expr(false, "a", v-expr(7), name-expr("a")).eval(empty-env) is ok(lpv-num(7))
  let-expr(true, "a", v-expr(7), name-expr("a")).eval(empty-env)
    is err(lpe-cant-bind-recursive-to-non-proc)

  complex-expr = let-expr(
    false,
    "a",
    v-expr(3),
    let-expr(
      false,
      "b",
      v-expr(4),
      if-expr(
        v-expr(false),
        name-expr("a"),
        name-expr("b"))))
  complex-expr.eval(empty-env) is ok(lpv-num(4))

  complex-expr2 = let-expr(
    false,
    "a",
    v-expr(3),
    let-expr(
      false,
      "b",
      v-expr(4),
      if-expr(
        v-expr(true),
        name-expr("a"),
        name-expr("b"))))
  complex-expr2.eval(empty-env) is ok(lpv-num(3))

  usr-proc-body = if-expr(name-expr("a"), v-expr(1), v-expr(2))
  l-expr = lambda-expr([list: "a"], usr-proc-body)
  l-expr.eval(empty-env)
    is ok(lpv-user-proc(user-proc(none, [list: "a"], empty-env, usr-proc-body)))
  call-expr(l-expr, [list: v-expr(true)]).eval(empty-env) is ok(lpv-num(1))
  call-expr(l-expr, [list: v-expr(false)]).eval(empty-env) is ok(lpv-num(2))

  call-expr(name-expr("add"), [list: v-expr(39), v-expr(81)]).eval(env-with-operators)
    is ok(lpv-num(120))
end

fun parse-literal-token(s :: String):
  tokenize(parse-string(s))
end

let-kw = parse-literal-token("let")
rec-kw = parse-literal-token("rec")
in-kw = parse-literal-token("in")
if-kw = parse-literal-token("if")
then-kw = parse-literal-token("then")
else-kw = parse-literal-token("else")
arrow-sign = parse-literal-token("->")
eq-sign = parse-literal-token("=")
l-paren = parse-literal-token("(")
r-paren = parse-literal-token(")")
l-curly = parse-literal-token("{")
r-curly = parse-literal-token("}")
true-lit = parse-literal-token("#t")
false-lit = parse-literal-token("#f")
number-lit = tokenize(parse-digit().many().map({(it): it.join-str("")}))
id = parse-letter()
  .or-else(parse-char("_"))
  .or-else(parse-char(":"))
  .flat-map({(chr):
    parse-letter()
      .or-else(parse-digit())
      .or-else(parse-char("_"))
      .or-else(parse-char(":"))
      .or-else(parse-char("-"))
      .or-else(parse-char("!"))
      .or-else(parse-char("?"))
      .many()
      .optional()
      .map({(rest):
        cases(Option) rest:
          | some(chars) => string-append(chr, chars.join-str(""))
          | none => chr
        end
      })
  }) ^ tokenize(_)

seq = {
  make2:
    lam(p0, p1):
      for flat-map(r0 from tokenize(p0)):
        for fmap(r1 from tokenize(p1)):
          {r0; r1}
        end
      end
    end,
  make3:
    lam(p0, p1, p2):
      for flat-map(r0 from tokenize(p0)):
        for flat-map(r1 from tokenize(p1)):
          for fmap(r2 from tokenize(p2)):
            {r0; r1; r2}
          end
        end
      end
    end,
  make4:
    lam(p0, p1, p2, p3):
      for flat-map(r0 from tokenize(p0)):
        for flat-map(r1 from tokenize(p1)):
          for flat-map(r2 from tokenize(p2)):
            for fmap(r3 from tokenize(p3)):
              {r0; r1; r2; r3}
            end
          end
        end
      end
    end,
  make5:
    lam(p0, p1, p2, p3, p4):
      for flat-map(r0 from tokenize(p0)):
        for flat-map(r1 from tokenize(p1)):
          for flat-map(r2 from tokenize(p2)):
            for flat-map(r3 from tokenize(p3)):
              for fmap(r4 from tokenize(p4)):
                {r0; r1; r2; r3; r4}
              end
            end
          end
        end
      end
    end
}

fun curled(p):
  [seq: l-curly, p, r-curly].map({({_; r; _}): r})
where:
  curled(id).try-parse(string-explode("{  x   }")) is ok({"x"; [list:]})
end

fun parenthesized(p):
  [seq: l-paren, p, r-paren].map({({_; r; _}): r})
where:
  parenthesized(id).try-parse(string-explode("(x)")) is ok({"x"; [list:]})
end

data LetProcParser:
    let-proc-parser with:
    method parse-true-literal(self):
      true-lit.map({(_): value-expr(lpv(true))})
    end,
    method parse-false-literal(self):
      false-lit.map({(_): value-expr(lpv(false))})
    end,
    method parse-number-literal(self):
      number-lit.map({(it): value-expr(lpv(string-to-number(it).or-else(0)))})
    end,
    method parse-name-expr(self):
      id.map({(it): name-expr(it)})
    end,
    method parse-lambda-expr(self):
      parse-args = id.many().optional()
      parse-seq = [seq: parse-args, arrow-sign, parser-ref(self.parse-expr)] ^ curled(_)
      for fmap({idents; _; body} from parse-seq):
        lambda-expr(idents.or-else([list:]), body)
      end
    end,
    method parse-call-expr(self):
      parse-expr-ref = parser-ref(self.parse-expr)
      parse-args = parse-expr-ref.many().optional()
      parse-seq = [seq: parse-expr-ref, parse-args] ^ parenthesized(_)
      for fmap({callee; args} from parse-seq):
        call-expr(callee, args.or-else([list:]))
      end
    end,
    method parse-if-expr(self):
      parse-expr-ref = parser-ref(self.parse-expr)
      parse-cond = [seq: if-kw, parse-expr-ref].map({(it): it.{1}})
      parse-then = [seq: then-kw, parse-expr-ref].map({(it): it.{1}})
      parse-else = [seq: else-kw, parse-expr-ref].map({(it): it.{1}})
      parse-seq = [seq: parse-cond, parse-then, parse-else] ^ parenthesized(_)
      for fmap({cond; thn; els} from parse-seq):
        if-expr(cond, thn, els)
      end
    end,
    method parse-let-expr(self):
      parse-expr-ref = parser-ref(self.parse-expr)
      for flat-map({_; r; n} from [seq: let-kw, rec-kw.optional(), id]):
        for fmap({_; v; _; body} from [seq: eq-sign, parse-expr-ref, in-kw, parse-expr-ref]):
          let-expr(is-some(r), n, v, body)
        end
      end ^ parenthesized(_)
    end,
    method parse-expr(self):
      self.parse-true-literal()
        .or-else(self.parse-false-literal())
        .or-else(self.parse-number-literal())
        .or-else(self.parse-if-expr())
        .or-else(self.parse-let-expr())
        .or-else(self.parse-lambda-expr())
        .or-else(self.parse-name-expr())
        .or-else(self.parse-call-expr())
    end
end

fun try-to-parse-program(s :: String):
  eof-parser = parse-eof({(): "expected eof!"})
  for fmap({expr; _} from [seq: let-proc-parser.parse-expr(), eof-parser]):
    expr
  end.try-parse(string-explode(s)).map({(it): it.{0}})
where:
  try-to-parse-program(
    ```
    (
      let rec fact = { x ->
        ( if
          (lt-eq x 1) then 1
          else (mul x (fact (sub x 1) ) )
        )
      } in (fact 4)
    )
    ```
  ) satisfies is-ok
end

test-cases = [list:
  ```
    ( print ( or #f #f ) )
  ```,
  ```
    ( print ( or #f #t ) )
  ```,
  ```
    ( print ( or #t #f ) )
  ```,
  ```
    ( print ( or #t #t ) )
  ```,
  ```
    ( print ( and #f #f ) )
  ```,
  ```
    ( print ( and #f #t ) )
  ```,
  ```
    ( print ( and #t #f ) )
  ```,
  ```
    ( print ( and #t #t ) )
  ```,
  ```
    ( print ( eq 0 1 ) )
  ```,
  ```
    ( print ( eq 0 0 ) )
  ```,
  ```
    ( let add2 = { x y -> ( add x y ) } in
    ( print ( add2 3 4 ) ) )
  ```,
  ```
    ( let carried-add = { x -> { y -> ( add x y ) } } in
    ( print ( ( carried-add 3 ) 4 ) ) )
  ```,
  ```
    ( let carried-or = { x -> { y -> ( or x y ) } } in
    ( let carried-and = { x -> { y -> ( and x y ) } } in
    ( print ( ( carried-and #f ) ( ( carried-or #t ) #f ) ) ) ) )
  ```,
  ```
    ( let f = { x -> ( sub x 11 ) } in
    ( print ( f ( f 77 ) ) ) )
  ```,
  ```
    (
      let rec fact = { x ->
        ( if
          (lt-eq x 1) then 1
          else (mul x (fact (sub x 1) ) )
        )
      } in ( print (fact 4) )
    )
  ```
]

check:
  for each(test-case from test-cases) block:
    prog = try-to-parse-program(test-case)
    prog satisfies is-ok

    res = for flat-map(expr from prog):
      expr.eval(env-with-operators)
    end

    res satisfies is-ok
  end
end
