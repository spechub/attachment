theory LinkedList_LinkedList_E1
imports Main
uses "$HETS_LIB/Isabelle/prelude"
begin

ML "Header.initialize
    [\"LL\", \"ga_injective___XColonXColon__\",
     \"ga_disjoint_XOSqBrXCSqBr___XColonXColon__\", \"concat_nil_List\",
     \"concat_NeList_List\", \"shift_brackets\", \"concat_singletons\",
     \"empty_list_is_linked\", \"single_element_list_is_linked\",
     \"head_elements\", \"a_link_implies_a_linked_list\"]"

typedecl Elem

datatype ListXOSqBrElemXCSqBr = XOSqBrXCSqBr ("[/ ]''") | X__XColonXColon__X "Elem" "ListXOSqBrElemXCSqBr" ("(_/ ::''/ _)" [57,56] 56)

consts
X_LinkedList :: "ListXOSqBrElemXCSqBr => bool" ("LinkedList/'(_')" [10] 999)
X__XPlusXPlus__X :: "ListXOSqBrElemXCSqBr => ListXOSqBrElemXCSqBr => ListXOSqBrElemXCSqBr" ("(_/ ++''/ _)" [54,55] 54)
X_link :: "Elem => Elem => bool" ("link/'(_,/ _')" [10,10] 999)

instance Elem:: type
by intro_classes
instance ListXOSqBrElemXCSqBr:: type
by intro_classes

axioms
ga_injective___XColonXColon__ :
"ALL XX1.
 ALL XX2.
 ALL Y1. ALL Y2. XX1 ::' XX2 = Y1 ::' Y2 = (XX1 = Y1 & XX2 = Y2)"

ga_disjoint_XOSqBrXCSqBr___XColonXColon__ [simp] :
"ALL Y1. ALL Y2. ~ [ ]' = Y1 ::' Y2"

concat_nil_List [simp] : "ALL K. [ ]' ++' K = K"

concat_NeList_List [simp](*added*) :
"ALL K. ALL L. ALL x. x ::' L ++' K = x ::' (L ++' K)"

shift_brackets :
"ALL L1.
 ALL L2.
 ALL x.
 ALL y.
 ALL z.
 x ::' (L1 ++' y ::' L2 ++' z ::' [ ]') =
 x ::' L1 ++' y ::' L2 ++' z ::' [ ]'"

concat_singletons [simp] :
"ALL x. ALL y. x ::' [ ]' ++' y ::' [ ]' = x ::' y ::' [ ]'"

empty_list_is_linked [simp] : "LinkedList([ ]')"

single_element_list_is_linked [simp] :
"ALL x. LinkedList(x ::' [ ]')"

head_elements [simp](*added*) :
"ALL K.
 ALL x.
 ALL y.
 LinkedList(x ::' y ::' K) = (link(x, y) & LinkedList(y ::' K))"

a_link_implies_a_linked_list :
"ALL x. ALL y. link(x, y) = LinkedList(x ::' y ::' [ ]')"

(*
(* the following adds the needed axioms to the simplifier with their names *)
declare ga_disjoint_XOSqBrXCSqBr___XColonXColon__ [simp del]
declare concat_nil_List [simp del] 
declare concat_nil_List [simp] 
declare concat_NeList_List [simp]
declare concat_singletons [simp del]
declare empty_list_is_linked [simp del]
declare single_element_list_is_linked [simp del]
declare single_element_list_is_linked [simp]
declare head_elements [simp]
*)

theorem LL :
"ALL y.
 ALL z.
 ALL L1.
 ALL L2.
 ALL x.
 LinkedList(x ::' (L1 ++' y ::' [ ]')) &
 LinkedList(y ::' (L2 ++' z ::' [ ]')) -->
 LinkedList(x ::' (L1 ++' y ::' L2 ++' z ::' [ ]'))"
apply(rule allI)
apply(rule allI)
apply(rule allI)
apply(induct_tac "L1")
apply(auto)
done

ML "Header.record \"LL\""

end
