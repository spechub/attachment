theory types_hc
imports "$HETS_LIB/Isabelle/HsHOLCF"
uses "$HETS_LIB/Isabelle/prelude"
begin

ML "Header.initialize []"

domain D1 = D1 (lazy D1_1::"dInt")
domain D2 = D2 (D2_1::"dInt")
types S = "dInt"
pcpodef N = "{x:: dInt . True}"
by auto

lemmas D1.rews [simp] D2.rews [simp] 
consts
d1 :: "D1 --> int lift"
d2 :: "D2 --> int lift"
n :: "N --> int lift"
s :: "S --> int lift"

defs
s_def :
"(s :: S --> int lift) ==
 Lam i. Def (42 :: int)"

d1_def :
"(d1 :: D1 --> int lift) ==
 Lam x . case x of D1 $ i => Def (42 :: int)"


d2_def :
"(d2 :: D2 --> int lift) ==
 Lam x . case x of D2 $ i => Def (42 :: int)"

n_def :
"(n :: N --> int lift) == Lam x . Def (42 :: int)"

lemma D1 : "D1 $ UU ~= UU"
apply (auto simp add: d1_def)
done

lemma D2 : "D2 $ UU == UU"
apply (auto simp add: d2_def)
done

lemma N : "Abs_N UU == UU"
apply (auto simp add: Abs_N_strict)
done

(* lemmas from the Haskell report, section 4.2.3 *)
lemma d1 : "d1 $$ UU == UU"
apply (auto simp add: d1_def)
done

lemma d2 : "d2 $$ UU == UU"
apply (auto simp add: d2_def)
done

lemma d2D2 : "d2 $$ (D2 $ UU) == UU"
apply (auto simp add: d2_def)
done

lemma n : "n $$ UU == Def (42 :: int)"
apply (auto simp add: n_def)
done

lemma nN : "n $$ (Abs_N UU) == Def (42 :: int)"
apply (auto simp add: n_def)
done

lemma d1D1 : "d1 $$ (D1 $ UU) == Def (42 :: int)"
apply (auto simp add: d1_def)
done

lemma s : "s $$ UU == Def (42 :: int)"
apply (auto simp add: s_def)
done

end
