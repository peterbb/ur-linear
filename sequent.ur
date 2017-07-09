datatype plus' a b =
    | Left  of a
    | Right of b

type lin ctx t = $ctx -> t

type lolli a b = a -> b
type one = {}
type tensor a b = a * b
type zero = variant []
type plus a b = plus' a b
type top = {}
type with a b = a * b
type ofcourse a = a

con fix (r :: {Type}) :: {Unit} = map (fn _ => ()) r

fun exec [gamma] [s] f x = f x

fun axiom [t] [n :: Name] : lin [n = t] t =
    fn r => r.n

fun cut [t] [s] [n :: Name] [gamma] [delta] [gamma ~ delta] 
        [[n] ~ fix (gamma ++ delta)]
        (d0 : lin gamma t) (d1 : lin ([n = t] ++ delta) s)
        : lin (gamma ++ delta) s 
        = 
        fn ctx =>
        let
            val ctx0 = ctx --- delta
            val ctx1 = ctx --- gamma
        in
            d1 (ctx1 ++ {n = d0 ctx0})
        end

fun lolli_left [n :: Name] [t0] [t1] [s] [gamma] [delta]
        [gamma ~ delta] [[n] ~ (gamma ++ delta)]
        (d0 : lin gamma t0) (d1 : lin ([n = t1] ++ delta) s)
    : lin (gamma ++ delta ++ [n = (t0 -> t1)]) s
    = fn ctx =>
        let 
            val f : t0 -> t1 = ctx.n
            val ctx : $(delta ++ gamma) = ctx -- n
            val ctx0 : $gamma = ctx --- delta
            val ctx1 : $delta = ctx --- gamma
            val x : t0 = d0 ctx0
            val fx : t1 = f x
            val y : s = d1 ({n = fx} ++ ctx1)
        in
            y
        end
    

fun lolli_right [n] [t0] [t1] [gamma] [[n]~gamma]
        (deriv : lin (gamma ++ [n = t0]) t1)
    : lin gamma (t0 -> t1)
    = fn (ctx : $gamma) (h : t0) =>
        let 
            val ctx : $(gamma ++ [n = t0]) = ctx ++ {n = h}
        in
            deriv ctx
        end

fun one_right = fn _ => ()

fun one_left [s] [gamma] [n :: Name] [[n]~gamma] d ctx =
    d (ctx -- n)
    
fun tensor_right [t0] [t1] [gamma0] [gamma1] [gamma0 ~ gamma1] d0 d1 ctx =
    (d0 (ctx --- gamma1), d1 (ctx --- gamma0))

fun tensor_left' [t0] [t1] [s] [gamma] [n :: Name] [m :: Name]
    [[n] ~ fix gamma] [[m] ~ [n] ++ fix gamma] d ctx =
        let
        val (x0, x1) = ctx.n
        in
            d ({n = x0, m = x1} ++ (ctx -- n))
        end

fun zero_left [s] [n :: Name] [gamma] [[n]~gamma] ctx =
    match ctx.n {}

fun plus_right0 [t0] [t1] [gamma] d ctx =
    Left (d ctx)

fun plus_right1 [t0] [t1] [gamma] d ctx =
    Right (d ctx)

fun plus_left [t0] [t1] [s] [n :: Name] [gamma] [[n] ~ fix gamma] d0 d1 ctx =
    let
        val x : plus' t0 t1 = ctx.n
        val ctx : $gamma = ctx -- n
    in
        case x of
        | Left x => d0 ({n = x} ++ ctx)
        | Right x => d1 ({n = x} ++ ctx)
    end

fun top_right [gamma] ctx = ()

fun with_right [t0] [t1] [gamma] d0 d1 ctx =
    (d0 ctx, d1 ctx)

fun with_left0 [t0] [t1] [s] [n :: Name] [gamma] [[n] ~ fix gamma] d ctx =
    d ({n = ctx.n.1} ++ (ctx -- n))

fun with_left1 [t0] [t1] [s] [n :: Name] [gamma] [[n] ~ fix gamma] d ctx =
    d ({n = ctx.n.2} ++ (ctx -- n))

fun ofcourse_right [t] d = d

fun ofcourse_left [t] [s] [gamma] [n :: Name] [[n] ~ gamma] d ctx = d ctx

fun ofcourse_weaken [t] [s] [gamma] [n :: Name] [[n] ~ gamma] d ctx =
    d (ctx -- n)
    
fun ofcourse_contract [t] [s] [gamma] [n :: Name] [m :: Name]
        [[n] ~ [m]] [[n, m] ~ fix gamma] d ctx =
    d ({m = ctx.n} ++ ctx)
    
