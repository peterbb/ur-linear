
con lin :: {Type} -> Type -> Type

val exec : gamma ::: {Type} -> s ::: Type -> (lin gamma s) -> $gamma -> s

con lolli    :: Type -> Type -> Type
con one      :: Type
con tensor   :: Type -> Type -> Type
con zero     :: Type
con plus     :: Type -> Type -> Type
con with     :: Type -> Type -> Type
con top      :: Type
con ofcourse :: Type -> Type

val axiom :
    t ::: Type -> 
    n :: Name ->
    lin [n = t] t

val cut :
    t ::: Type -> s ::: Type -> n :: Name ->
    gamma ::: {Type} -> delta ::: {Type} ->
    [gamma ~ delta] => [[n] ~ map (fn _ => ()) (gamma ++ delta)] =>
    lin gamma t ->
    lin ([n = t] ++ delta) s ->
    lin (gamma ++ delta) s

val lolli_right : 
    n ::: Name -> t0 ::: Type -> t1 ::: Type -> gamma ::: {Type} ->
    [[n] ~ gamma] =>
    lin (gamma ++ [n = t0]) t1 ->
    lin gamma (lolli t0 t1)

val lolli_left :
    n :: Name -> t0 ::: Type -> t1 ::: Type -> s ::: Type ->
    gamma ::: {Type} -> delta ::: {Type} ->
    [gamma ~ delta] => [[n] ~ (gamma ++ delta)] =>
    lin gamma t0 ->
    lin ([n = t1] ++ delta) s ->
    lin (gamma ++ delta ++ [n = lolli t0 t1]) s


val one_right :
    lin [] one

val one_left :
    s ::: Type -> gamma ::: {Type} -> n :: Name -> [[n] ~ gamma] =>
    lin gamma s ->
    lin ([n = one] ++ gamma) s

val tensor_right :
    t0 ::: Type -> t1 ::: Type ->
    gamma0 ::: {Type} -> gamma1 ::: {Type} -> [gamma0 ~ gamma1] =>
    lin gamma0 t0 -> lin gamma1 t1 ->
    lin (gamma0 ++ gamma1) (tensor t0 t1)

(*
val tensor_l :
    t0 ::: Type -> t1 ::: Type -> s ::: Type -> gamma ::: {Type} ->
    n ::: Name -> [[n] ~ gamma] =>
    (m ::: Name -> [[m] ~ [n] ++ gamma] =>
        lin ([n = t0, m = t1] ++ gamma) s) ->
    lin ([n = tensor t0 t1] ++ gamma) s
*)

val tensor_left' :
    t0 ::: Type -> t1 ::: Type -> s ::: Type -> gamma ::: {Type} ->
    n :: Name -> m :: Name ->
    [[n] ~ map (fn _ => ()) gamma]
    => [[m] ~ [n] ++ map (fn _ => ()) gamma] =>
    lin ([n = t0, m = t1] ++ gamma) s ->
    lin ([n = tensor t0 t1] ++ gamma) s
    

val zero_left :
    s ::: Type -> n :: Name -> gamma ::: {Type} -> [[n] ~ gamma] =>
    lin ([n = zero] ++ gamma) s

val plus_right0 : 
    t0 ::: Type -> t1 ::: Type -> gamma ::: {Type} ->
    lin gamma t0 ->
    lin gamma (plus t0 t1)

val plus_right1 : 
    t0 ::: Type -> t1 ::: Type -> gamma ::: {Type} ->
    lin gamma t1 ->
    lin gamma (plus t0 t1)

val plus_left :
    t0 ::: Type -> t1 ::: Type -> s ::: Type ->
    n :: Name -> gamma ::: {Type} ->
    [[n] ~ map (fn _ => ()) gamma] =>
    lin ([n = t0] ++ gamma) s ->
    lin ([n = t1] ++ gamma) s ->
    lin ([n = plus t0 t1] ++ gamma) s

val top_right :
    gamma ::: {Type} ->
    lin gamma top

val with_right :
    t0 ::: Type -> t1 ::: Type -> gamma ::: {Type} ->
    lin gamma t0 ->
    lin gamma t1 ->
    lin gamma (with t0 t1)

val with_left0 :
    t0 ::: Type -> t1 ::: Type -> s ::: Type ->
    n :: Name -> gamma ::: {Type} ->
    [[n] ~ map (fn _ => ()) gamma] =>
    lin ([n = t0] ++ gamma) s ->
    lin ([n = with t0 t1] ++ gamma) s

val with_left1 :
    t0 ::: Type -> t1 ::: Type -> s ::: Type ->
    n :: Name -> gamma ::: {Type} ->
    [[n] ~ map (fn _ => ()) gamma] =>
    lin ([n = t1] ++ gamma) s ->
    lin ([n = with t0 t1] ++ gamma) s

val ofcourse_right :
    t ::: Type ->
    lin [] t ->
    lin [] (ofcourse t)

val ofcourse_left : 
    t ::: Type -> s ::: Type -> gamma ::: {Type} -> n :: Name ->
    [[n] ~ gamma] =>
    lin ([n = t] ++ gamma) s ->
    lin ([n = ofcourse t] ++ gamma) s

val ofcourse_weaken :
    t ::: Type -> s ::: Type -> gamma ::: {Type} -> n :: Name ->
    [[n] ~ gamma] =>
    lin gamma s ->
    lin ([n = ofcourse t] ++ gamma) s

(* TODO: Want binding version if possible. *)
val ofcourse_contract :
    t ::: Type -> s ::: Type -> gamma ::: {Type} -> n :: Name -> m :: Name ->
    [[n] ~ [m]] => [[n, m] ~ map (fn _ => ()) gamma] =>
    lin ([n = ofcourse t, m = ofcourse t] ++ gamma) s ->
    lin ([n = ofcourse t] ++ gamma) s
