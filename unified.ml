exception NotImplemented

let wrap f a = Some(f a)
let compose f g = fun x -> f (g x)

(** Simple types **)
(* Variables use https://en.wikipedia.org/wiki/De_Bruijn_index *)

type typ = Bool | Num | Lst of typ
        | Unit | Prod of typ * typ (* | Sum of typ * typ | Arrow of typ * typ *)
        (* | Rec of typ | ForAll of typ | TVar of int *)

type exp = B of bool | N of int | L of typ * exp list
        | Nil | Pair of exp * exp | ProjL of exp | ProjR of exp 
        (* | InjL of exp * typ | InjR of typ * exp | Case of exp * exp * exp *)
        (* | Fun of typ * exp | Ap of exp * exp | Let of exp * exp *)
        (* | Roll of typ * exp | UnRoll of exp | Fix of typ * exp *)
        (* | TFun of exp | TAp of typ * exp *)
        | Var of int
        (* FIX: maybe we should define binary operators? *)
        | And of exp * exp | Or of exp * exp
        | Eq of exp * exp | Less of exp * exp
        (* FIX: should we use recursive + function types *)
        | Len of exp | Zip of exp * exp | Ls of exp | Rs of exp

let rec tmap f t =
        let tmap = tmap f in
        match f t with 
        | None -> begin match t with
                | (Unit | Bool | Num) as t -> t
                | Lst t -> Lst (tmap t)
                | Prod(l, r) -> Prod(tmap l, tmap r)
                (* | Sum(l, r) -> Sum(tmap l, tmap r) *)
                (* | Arrow(l, r) -> Arrow(tmap l, tmap r) *)
                (* | Rec t -> Rec(tmap t) *)
                (* | ForAll t -> ForAll(tmap t) *)
                (* | TVar i -> TVar i *)
                end
        | Some t -> t

let rec emap tf f e =
        let tmap = tmap tf in
        let emap = emap tf f in
        match f e with
        | None -> begin match e with
                (** Primitives **)
                | (B _ | N _) as e -> e
                | L(t, l) -> L(tmap t, List.map emap l)
                (** Product **)
                | Nil -> Nil
                | Pair(l, r) -> Pair(emap l, emap r)
                | ProjL e -> ProjL(emap e) | ProjR e -> ProjR(emap e)
                (** Sum **)
                (* | InjL(e, t) -> InjL(emap e, tmap t) *)
                (* | InjR(t, e) -> InjR(tmap t, emap e) *)
                (* | Case(e, le, re) -> Case(emap e, emap le, emap re) *)
                (** Arrow **)
                (* | Fun(t, e) -> Fun(tmap t, emap e) *)
                (* | Ap(f, e) -> Ap(emap f, emap e) *)
                (* | Let(v, b) -> Let(emap v, emap b) *)
                (** Recursive **)
                (* | Roll(t, e) -> Roll(tmap t, emap e) *)
                (* | UnRoll e -> UnRoll(emap e) *)
                (* | Fix(t, e) -> Fix(tmap t, emap e) *)
                (** Functions **)
                (* | TFun e -> TFun(emap e) *)
                (* | TAp(t, e) -> TAp(tmap t, emap e) *)
                (** Variable **)
                | Var i -> Var i
                (* FIX: maybe we should define binary operators? *)
                | And(l, r) -> And(emap l, emap r)
                | Or(l, r) -> Or(emap l, emap r) 
                | Eq(l, r) -> Eq(emap l, emap r)
                | Less(l, r) -> Less(emap l, emap r)
                (* FIX: should we use recursive + function types *)
                | Len e -> Len(emap e)
                | Zip(l, r) -> Zip(emap l, emap r)
                | Ls e -> Ls(emap e)
                | Rs e -> Rs(emap e)
                end
        | Some e -> e

let rec tsubt replacement ?(v = 0) =
        (* let tsubt i = tsubt replacement ~v:(v + i) in *)
        tmap begin function
                (* | Rec u -> Some(Rec(tsubt 1 u)) *)
                (* | ForAll u -> Some(ForAll(tsubt 1 u)) *)
                (* | TVar i when i = v -> Some replacement *)
                (* | TVar i when i > v -> Some(TVar(i - 1)) *)
                | _ -> None
        end

let rec tunsubt t ?(v = 0) =
        (* let tunsubt i = tunsubt t ~v:(v + i) in *)
        (* TODO: Does this make sense for non-Rec t?*)
        tmap begin function
                (* | u when u = t -> Some(TVar v) *)
                (* | Rec u -> Some(Rec(tunsubt 1 u)) *)
                (* | ForAll u -> Some(ForAll(tunsubt 1 u)) *)
                | _ -> None
        end

let rec esube replacement ?(v = 0) =
        (* let esube i = esube replacement ~v:(v + i) in *)
        emap (fun t -> Some t) begin function
                (* | Fun(t, e) -> Some(Fun(t, esube 1 e)) *)
                (* | Let(v, b) -> Some(Let(esube 0 v, esube 1 b)) *)
                (* | Var i when i = v -> Some replacement *)
                (* | Var i when i > v -> Some(Var(i - 1)) *)
                | _ -> None
        end

let rec tsube replacement ?(v = 0) =
        (* let tsube i = tsube replacement ~v:(v + i) in *)
        emap (wrap (tsubt replacement ~v)) begin function
                (* | TFun e -> Some(TFun(tsube 1 e)) *)
                | _ -> None
        end


exception InvalidT
let rec tvalid ?(d = 0) =
        (* let tvalid i = tvalid ~d:(d + i) in *)
        tmap begin function
                (* | Rec u -> Some(Rec(tvalid 1 u)) *)
                (* | ForAll u -> Some(ForAll(tvalid 1 u)) *)
                (* | TVar i when i < d -> Some(TVar i) *)
                (* | TVar _ -> raise InvalidT *)
                | _ -> None
        end

exception IllTyped
let rec syn ?(delta = 0) ?(tau = []) e = 
        let syn ?(d = 0) ?(ts = []) =
                syn ~delta:(delta + d) ~tau:(ts @ tau) in
        let tmatch t u = if t = u then t else raise IllTyped in
        let tvalid = tvalid ~d:delta in
        try match e with
        (** Primitives **)
        | B _ -> Bool | N _ -> Num
        | L(t, l) -> Lst (List.fold_left tmatch (tvalid t) (List.map syn l))
        (** Product **)
        | Nil -> Unit
        | Pair(l, r) -> Prod(syn l, syn r)
        | ProjL e -> let Prod(l, _) = syn e in l
        | ProjR e -> let Prod(_, r) = syn e in r
        (** Sum **)
        (* | InjL(e, t) -> Sum(syn e, tvalid t) *)
        (* | InjR(t, e) -> Sum(tvalid t, syn e) *)
        (* | Case(e, le, re) -> let Sum(lt, rt) = syn e in
                tmatch (syn ~ts:[lt] le) (syn ~ts:[rt] re) *)
        (** Arrow **)
        (* | Fun(t, e) -> Arrow(tvalid t, syn ~ts:[t] e) *)
        (* | Ap(f, e) -> let Arrow(i, o) = syn f in tmatch i (syn e) *)
        (* | Let(v, b) -> syn ~ts:[syn v] b *)
        (** Recursive **)
        (* | Roll(t, e) -> Rec(tunsubt t (syn e)) *)
        (* | UnRoll e -> let Rec t = syn e in tsubt (Rec t) t *)
        (* | Fix(t, e) -> tmatch (tvalid t) (syn ~ts:[t] e) *)
        (** Functions **)
        (* | TFun e -> ForAll (syn ~d:1 e) *)
        (* | TAp(t, e) -> tsubt (tvalid t) (syn e) *)
        (** Variable **)
        | Var i -> begin match List.nth_opt tau i with
                | Some t -> t | None -> raise IllTyped
                end
        (* FIX: maybe we should define binary operators? *)
        | And(l, r) | Or(l, r) -> let Bool, Bool = syn l, syn r in Bool
        | Eq(l, r) | Less(l, r) -> let Num, Num = syn l, syn r in Bool
        (* FIX: should we use recursive + function types *)
        | Len e -> let Lst _ = syn e in Num
        | Zip(l, r) -> let Lst l, Lst r = syn l, syn r in Lst(Prod(l, r))
        | Ls e -> let Lst(Prod(l, _)) = syn e in Lst l
        | Rs e -> let Lst(Prod(_, r)) = syn e in Lst r
        with Match_failure _ -> raise IllTyped

exception NotValue
let rec value = emap (wrap tvalid) begin function
        | B _ | N _ | L _ -> None
        | Nil | Pair _ (* | InjL _ | InjR _ *) (* | Roll _ *) -> None
        (* | Fun(t, e) -> Some(Fun(tvalid t, let _ = syn ~tau:[t] e in e)) *)
        (* | TFun e -> Some(TFun(let _ = syn ~delta:1 e in e)) *)
        | _ -> raise NotValue
        end

exception RunError
let rec eval ?(vars = []) e = 
        let eval ?(vs = []) = eval ~vars:(vs @ vars) in
        try match e with
        (** Primitives **)
        | B _ | N _ | L _ -> e
        (** Product **)
        | Nil -> Nil
        | Pair(l, r) -> Pair(eval l, eval r)
        | ProjL e -> let Pair(l, _) = eval e in l
        | ProjR e -> let Pair(_, r) = eval e in r
        (** Sum **)
        (* | InjL(e, t) -> InjL(eval e, t) *)
        (* | InjR(t, e) -> InjR(t, eval e) *)
        (* | Case(e, le, re) -> begin match eval e with
                | InjL(e, t) -> eval ~vs:[e] le
                | InjR(t, e) -> eval ~vs:[e] re
                | _ -> raise RunError
                end *)
        (** Arrow **)
        (* | Fun(t, e) -> Fun(t, e) *)
        (* | Ap(f, e) -> let Fun(_, b) = eval f in eval ~vs:[eval e] b *)
        (* | Let(v, b) -> eval ~vs:[eval v] b *)
        (** Recursive **)
        (* | Roll(t, e) -> Roll(t, eval e) *)
        (* | UnRoll e -> let Roll(_, e) = eval e in e *)
        (* | Fix(t, b) -> eval (esube e b) *)
        (** Functions **)
        (* | TFun e -> TFun e *)
        (* | TAp(t, e) -> let TFun e = e in tsube t e *)
        (** Variable **)
        | Var i -> begin match List.nth_opt vars i with
                | Some t -> t | None -> raise RunError
                end
        (* FIX: maybe we should define binary operators? *)
        | And(l, r) -> let B l, B r = eval l, eval r in B(l && r)
        | Or(l, r) -> let B l, B r = eval l, eval r in B(l || r)
        | Eq(l, r) -> let N l, N r = eval l, eval r in B(l = r)
        | Less(l, r) -> let N l, N r = eval l, eval r in B(l < r)
        (* FIX: should we use recursive + function types *)
        | Len e -> let L(_, ls) = eval e in N(List.length ls)
        | Zip(l, r) -> let L(tl, ll), L(tr, rl) = eval l, eval r in
                L(Prod(tl, tr), List.map2 (fun l r -> Pair(l, r)) ll rl)
        | Ls e -> let L(Prod(t, _), ls) = eval e in
                L(t, List.map (fun (Pair(l, _)) -> l) ls)
        | Rs e -> let L(Prod(_, t), ls) = eval e in
                L(t, List.map (fun (Pair(_, r)) -> r) ls)
        with Match_failure _ -> raise RunError

(** Refinement types **)

type refine = Simple of typ
        | Refine of typ * exp

let rec rmap tf ef f r = 
        let rmap = rmap tf ef f in
        let emap = emap tf ef in
        let tmap = tmap tf in
        match f r with
        | None -> begin match r with
                | Simple t -> Simple(tmap t)
                | Refine(t, phi) -> Refine(tmap t, emap phi)
                end
        | Some r -> r

let rec rvalid = rmap (wrap tvalid) begin function
                (* Only destructuring for now *)
                | ProjL _ | ProjR _ (* | Case _ | Ap _ | UnRoll _ | TAp _ *) -> None
                | And _  | Or _ | Eq _ | Less _ -> None
                | _ -> raise NotImplemented
        end
;;

(** Simplification **)

let lcs ls =
        let rec lcp = function
                | (h :: r) :: tl when List.for_all ((=) h) r -> h :: lcp tl
                | _ -> [] in
        List.rev (lcp (List.map List.rev ls))

let common es =
        let rec common cs =
                (* Should this ever return true? *)
                (false, lcs (List.map (compose snd recurse) cs))
        and recurse p = match p with
                | Var _ -> (true, [p])
                | ProjL c | ProjR c -> unary p c
                | _ -> raise NotImplemented
        and unary p c = match recurse c with
                | true, l -> (true, p :: l)
                | false, l -> (false, l) in
        match snd (common es) with
                | hd::_ -> Some hd
                | [] -> None

let rec names ?pre:(x = Var 0) (t: typ) = match t with
        | Prod(l, r) -> names ~pre:(ProjL x) l @ names ~pre:(ProjR x) r
        | Unit | Bool | Num | Lst _ -> [x]


let rec simplify t phi = (??)