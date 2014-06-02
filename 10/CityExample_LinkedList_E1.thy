theory CityExample_LinkedList_E1
imports Main
uses "$HETS_LIB/Isabelle/prelude"
begin

ML "Header.initialize
    [\"ifXSlash_32aXSlash_32linkedXSlash_32listXSlash_32endsXSlash_32inXSlash_32anXSlash_32elementXSlash_32whichXSlash_32isXSlash_32theXSlash_32headXSlash_32ofXSlash_32anotherXSlash_32linkedXSlash_32listXSlash_32theXSlash_32concatanationXSlash_32ofXSlash_32theseXSlash_32isXSlash_32alsoXSlash_32linked\",
     \"ga_injective___XColonXColon__\",
     \"ga_disjoint_XOSqBrXCSqBr___XColonXColon__\", \"concat_nil_List\",
     \"concat_NeList_List\", \"shiftXSlash_32brackets\",
     \"concatXSlash_32singletons\",
     \"emptyXSlash_32listXSlash_32isXSlash_32linked\",
     \"singleXSlash_32elementXSlash_32listXSlash_32isXSlash_32linked\",
     \"headXSlash_32elementsXSlash_32areXSlash_32linkedXSlash_32andXSlash_32theXSlash_32restXSlash_32isXSlash_32linkedXSlash_32XEqXGtXSlash_32theXSlash_32wholeXSlash_32isXSlash_32linked\",
     \"aXSlash_32linkXSlash_32impliesXSlash_32aXSlash_32linkedXSlash_32list\"]"

typedecl Elem

datatype ListXOSqBrElemXCSqBr = XOSqBrXCSqBr ("[/ ]''") | X__XColonXColon__X "Elem" "ListXOSqBrElemXCSqBr" ("(_/ ::''/ _)" [54,54] 52)

consts
X_LinkedList :: "ListXOSqBrElemXCSqBr => bool" ("LinkedList/'(_')" [10] 999)
X__XPlusXPlus__X :: "ListXOSqBrElemXCSqBr => ListXOSqBrElemXCSqBr => ListXOSqBrElemXCSqBr" ("(_/ ++''/ _)" [54,54] 52)
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

concat_NeList_List [simp]:
"ALL K. ALL L. ALL x. (x ::' L) ++' K = x ::' (L ++' K)"

shiftXSlash_32brackets :
"ALL L1.
 ALL L2.
 ALL x.
 ALL y.
 ALL z.
 x ::' ((L1 ++' (y ::' L2)) ++' (z ::' [ ]')) =
 ((x ::' L1) ++' (y ::' L2)) ++' (z ::' [ ]')"

concatXSlash_32singletons [simp] :
"ALL x. ALL y. (x ::' [ ]') ++' (y ::' [ ]') = x ::' (y ::' [ ]')"

emptyXSlash_32listXSlash_32isXSlash_32linked [simp] :
"LinkedList([ ]')"

singleXSlash_32elementXSlash_32listXSlash_32isXSlash_32linked [simp] :
"ALL x. LinkedList(x ::' [ ]')"

headXSlash_32elementsXSlash_32areXSlash_32linkedXSlash_32andXSlash_32theXSlash_32restXSlash_32isXSlash_32linkedXSlash_32XEqXGtXSlash_32theXSlash_32wholeXSlash_32isXSlash_32linked [simp]:
"ALL K.
 ALL x.
 ALL y.
 LinkedList(x ::' (y ::' K)) = (link(x, y) & LinkedList(y ::' K))"

aXSlash_32linkXSlash_32impliesXSlash_32aXSlash_32linkedXSlash_32list :
"ALL x. ALL y. link(x, y) = LinkedList(x ::' (y ::' [ ]'))"

theorem ifXSlash_32aXSlash_32linkedXSlash_32listXSlash_32endsXSlash_32inXSlash_32anXSlash_32elementXSlash_32whichXSlash_32isXSlash_32theXSlash_32headXSlash_32ofXSlash_32anotherXSlash_32linkedXSlash_32listXSlash_32theXSlash_32concatanationXSlash_32ofXSlash_32theseXSlash_32isXSlash_32alsoXSlash_32linked :
"ALL y.
 ALL z.
 ALL L1.
 ALL L2.
 ALL x.
 LinkedList(x ::' (L1 ++' (y ::' [ ]'))) &
 LinkedList(y ::' (L2 ++' (z ::' [ ]'))) -->
 LinkedList(x ::' ((L1 ++' (y ::' L2)) ++' (z ::' [ ]')))"
apply(rule allI)
apply(rule allI)
apply(rule allI)
apply(induct_tac "L1")
apply(auto)
done

ML "Header.record
    \"ifXSlash_32aXSlash_32linkedXSlash_32listXSlash_32endsXSlash_32inXSlash_32anXSlash_32elementXSlash_32whichXSlash_32isXSlash_32theXSlash_32headXSlash_32ofXSlash_32anotherXSlash_32linkedXSlash_32listXSlash_32theXSlash_32concatanationXSlash_32ofXSlash_32theseXSlash_32isXSlash_32alsoXSlash_32linked\""

end
