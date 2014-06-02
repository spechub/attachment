theory Isa_S
imports Main
uses "$HETS_LIB/Isabelle/prelude"
begin

ML "Header.initialize [\"Ax1\", \"Ax2\", \"Ax3\"]"

typedecl s

consts
a :: "s"
b :: "s"
c :: "s"

axioms
Ax1 [rule_format] : "a = b"

Ax2 [rule_format] : "b = c"

declare Ax1 [simp]
declare Ax2 [simp]

theorem Ax3 : "a = c"
by (auto)

ML "Header.record \"Ax3\""

end
