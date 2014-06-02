theory subsorts
imports Main
begin
typedecl the_sorts
consts
  subsorts :: "(the_sorts * the_sorts) set"
  sort_of :: "'a => the_sorts"
  injection :: "'a => 'b"

axioms
  subsorts_reflexive : "reflexive subsorts"
  subsort_transitive : "trans subsorts"
  sort_of_invarianve : "sort_of (x::'a) = sort_of (y::'a)"
  injective : "[|(sort_of (x::'a), sort_of (z::'c)) : subsorts; sort_of (x::'a) = sort_of (y::'a); injection x = injection y|] ==> x=y"
  transitive : "[|(sort_of (x::'a),sort_of (y::'b)) : subsorts; (sort_of (y::'b),sort_of (z::'c)) : subsorts|]
                 ==> (injection :: 'b=>'c)((injection :: 'a=>'b) x) = (injection :: 'a=>'c) x"
  identity : "(injection :: 'a=>'a) x = x"

