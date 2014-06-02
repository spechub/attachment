theory IsabelleCodingErrors_OptionsModel2_E1
imports Main
uses "$HETS_LIB/Isabelle/prelude"
begin

ML "Header.initialize
    [\"all_elements_of_OptXOSqBrAnnXCSqBr\", \"ga_injective_Just\",
     \"ga_disjoint_Nothing_Just\", \"only_one_element_in_Ann\"]"

datatype Sing = XHash ("#''")
datatype OptXOSqBrSingXCSqBr = X_Just "Sing" ("Just/'(_')" [10] 999) | Nothing

instance OptXOSqBrSingXCSqBr:: type
by intro_classes
instance Sing:: type
by intro_classes

axioms
ga_injective_Just :
"ALL XX1. ALL Y1. Just(XX1) = Just(Y1) = (XX1 = Y1)"

ga_disjoint_Nothing_Just : "ALL Y1. ~ Nothing = Just(Y1)"

only_one_element_in_Ann : "ALL x. x = #'"

declare ga_injective_Just [simp]
declare ga_disjoint_Nothing_Just [simp]

lemma L1 :
"y = Nothing | y = Just(#')"
proof (induct y)
  case Nothing show ?case by simp
next
  case X_Just from only_one_element_in_Ann show ?case by simp
qed


theorem all_elements_of_OptXOSqBrAnnXCSqBr :
"ALL y. y = Nothing | y = Just(#')"
proof -
  from L1 show ?thesis ..
qed


ML "Header.record \"all_elements_of_OptXOSqBrAnnXCSqBr\""

end
