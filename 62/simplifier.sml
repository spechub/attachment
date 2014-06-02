(* *********************************************************************** *)
(*									   *)
(* Project: HOL-CASL 							   *)
(* Author: Till Mossakowski, University of Bremen		           *)
(* Date: 17.09.2001			 			           *)
(* Purpose of this file: Generation of simplifier sets for HOL-CASL 	   *)
(*			 						   *)	
(*									   *)
(* *********************************************************************** *)


(* At the moment, just the universally quantified conditional equations
   are added to the simplifier set.
*)

(* todo
   ga_membership_xxx raus!
   ga_strictness_xxx rein!
   overload-Axiome fuer strikte Erw. rein!

   In the future, a termination analysis should be done.
  Overload-Axiome als Substitutionsregeln in Simplifier
  Kongruenz-Axiome richtig in Default-Simplifier einbauen
   arg_cong mit read_instantiate_sg fuer alle Funktionen/Praedikate
   instantiieren und dann mit addcongs in den Simplifier
*)

(*
fun is_eq (atom (strong_equation _)) = true
  | is_eq (atom (existl_equation _)) = true
  | is_eq _ = false

fun is_eeq (atom (strong_equation _)) = true
  | is_eeq _ = false

fun is_cond_eq (quantification (forall,_,phi)) =
    is_cond_eq phi
  | is_cond_eq (quantification _) = false
  | is_cond_eq (atom (strong_equation _)) = true
  | is_cond_eq (atom (existl_equation _)) = true
  | is_cond_eq (conjunction phis) = Library.forall is_cond_eq phis
  | is_cond_eq (implication (phi,psi)) =
    is_eeq phi andalso is_eq psi
  | is_cond_eq (equivalence (phi,psi)) =
    is_eeq phi andalso is_eeq psi
  | is_cond_eq (pos_FORMULA (_,phi)) =
    is_cond_eq phi
  | is_cond_eq _ = false
*)


fun (*is_conj_atomic (Const ("op &",_) $ t1 $ t2) =
    is_conj_atomic t1 andalso is_conj_atomic t2
  | *) is_conj_atomic (Const (c,_)) = true (* "." mem (explode c) *)
  | is_conj_atomic (Free _) = true
  | is_conj_atomic (Var _) = true
  | is_conj_atomic (Bound _) = true
  | is_conj_atomic (Abs _) = false
  | is_conj_atomic (t1 $ t2) = is_conj_atomic t1 andalso is_conj_atomic t2;

fun is_conj_atom (Const ("Trueprop",_) $ (Const ("op =",_) $ t1 $ t2)) = 
    is_conj_atomic t1 andalso is_conj_atomic t2
  | is_conj_atom t = is_conj_atomic t;

fun is_conj_atom_or_negatom (Const ("Not",_) $ t) =
    is_conj_atom t
  | is_conj_atom_or_negatom t = is_conj_atom t

fun is_cond_eq (Const ("==>",_) $ e1 $ e2) =
    is_conj_atom e1 andalso is_cond_eq e2
  | is_cond_eq t = is_conj_atom_or_negatom t;


fun term_of thm = #prop(rep_thm thm);

fun is_simp (n,thm) =
    case snd (take_prefix  (fn x => x<>".") (explode n)) of
     "."::"g"::"a"::"_"::_ => false
    | _ => is_cond_eq (term_of thm);


fun AddsimpAll () = 
    Addsimps (map snd (filter is_simp (axioms_of (the_context ()))))

fun PrintsimpAll () =
    map fst (filter is_simp (axioms_of (the_context ())));


fun AddSimpParents () =
  let val thy = the_context()
      val thys = parents_of thy
      fun change_ss (ss,_) = foldl merge_ss (ss,(map simpset_of thys))
  in (Simplifier.change_simpset_of change_ss () thy;())
  end

fun is_overload (n,thm) =
    case snd (take_prefix  (fn x => x<>".") (explode n)) of
     "."::"g"::"a"::"_"::"o"::"v"::"e"::"r"::"l"::_ => true
    | _ => false;

fun is_emb_trans (n,thm) =
    case snd (take_prefix  (fn x => x<>".") (explode n)) of
     "."::"g"::"a"::"_"::"t"::"r"::"a"::"n"::"s"::"i"::"t"::_ => true
    | _ => false;

fun is_noq s = 
    let val (_,s1) = take_prefix (fn x => not (x="_")) s
        val (_,s2) = take_prefix (fn x => x="_") s1
    in (case s2 of 
        "?"::_ => false
        | _ => true)
    end;

fun is_emb_trans_noq (n,thm) =
    case snd (take_prefix  (fn x => x<>".") (explode n)) of
     "."::"g"::"a"::"_"::"t"::"r"::"a"::"n"::"s"::"i"::"t"::"i"::"v"::"i"::"t"::"y"::"_"::rest
                  => is_noq rest
    | _ => false;

fun ax_eq ((n1,_),(n2,_)) = n1=n2

fun overload_down () =
  let val thy = the_context()
      val thys = ancestors_of thy
      val ss = simpset_of thy
      val ax = flat (map axioms_of (thy::thys))
      val overload_ax =  map snd (Utils.remove_dups_eq ax_eq (filter is_overload ax))
      (*val _ = seq (writeln o fst) (filter is_overload ax)*)
      val trans_ax = map snd (filter is_emb_trans ax)
      val trans_noq_ax = map snd (filter is_emb_trans_noq ax)
      val rev_trans_noq_ax = map (fn x => x RS sym) trans_noq_ax
      (*val _ = seq (writeln o fst) (filter is_emb_trans_noq ax)*)
  in simpset() delsimps trans_ax addsimps (overload_ax@rev_trans_noq_ax)
  end;

fun overload_up () =
  let val thy = the_context()
      val thys = ancestors_of thy
      val ss = simpset_of thy
      val ax = flat (map axioms_of (thy::thys))
      val overload_ax = map (fn ax => ax RS sym) (map snd (Utils.remove_dups_eq ax_eq (filter is_overload ax)))
      (*val _ = seq (writeln o fst) (filter is_overload ax)*)
      val trans_ax = map snd (filter is_emb_trans ax)
      val trans_noq_ax = map snd (filter is_emb_trans_noq ax)
      val rev_trans_noq_ax = map (fn x => x RS sym) trans_noq_ax
      (*val _ = seq (writeln o fst) (filter is_emb_trans_noq ax)*)
  in simpset() delsimps trans_ax addsimps (overload_ax @rev_trans_noq_ax)
  end;
