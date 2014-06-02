theory subsorts_sp
imports "$HETS_LIB/Isabelle/MainHC"
uses "$HETS_LIB/Isabelle/prelude"
begin

ML "Header.initialize
    [\"ga_embedding_injectivity\", \"ga_projection_injectivity\",
     \"ga_projection\", \"ga_embedding_injectivity_1\",
     \"ga_projection_injectivity_1\", \"ga_projection_1\",
     \"ga_embedding_injectivity_2\", \"ga_projection_injectivity_2\",
     \"ga_projection_2\", \"ga_embedding_injectivity_3\",
     \"ga_projection_injectivity_3\", \"ga_projection_3\",
     \"ga_transitivity\", \"ga_transitivity_1\", \"ga_transitivity_2\"]"

typedecl S
typedecl T
typedecl U
typedecl V
typedecl W

consts
gn_injX1 :: "S => T" ("gn'_inj/'(_')" [10] 999)
gn_injX10 :: "V => W" ("gn'_inj'_9/'(_')" [10] 999)
gn_injX2 :: "S => U" ("gn'_inj''/'(_')" [10] 999)
gn_injX3 :: "S => V" ("gn'_inj''''/'(_')" [10] 999)
gn_injX4 :: "S => W" ("gn'_inj'_3/'(_')" [10] 999)
gn_injX5 :: "T => U" ("gn'_inj'_4/'(_')" [10] 999)
gn_injX6 :: "T => V" ("gn'_inj'_5/'(_')" [10] 999)
gn_injX7 :: "T => W" ("gn'_inj'_6/'(_')" [10] 999)
gn_injX8 :: "U => V" ("gn'_inj'_7/'(_')" [10] 999)
gn_injX9 :: "U => W" ("gn'_inj'_8/'(_')" [10] 999)
gn_projX1 :: "T => S option" ("gn'_proj/'(_')" [10] 999)
gn_projX10 :: "W => V option" ("gn'_proj'_9/'(_')" [10] 999)
gn_projX2 :: "U => S option" ("gn'_proj''/'(_')" [10] 999)
gn_projX3 :: "U => T option" ("gn'_proj''''/'(_')" [10] 999)
gn_projX4 :: "V => S option" ("gn'_proj'_3/'(_')" [10] 999)
gn_projX5 :: "V => T option" ("gn'_proj'_4/'(_')" [10] 999)
gn_projX6 :: "V => U option" ("gn'_proj'_5/'(_')" [10] 999)
gn_projX7 :: "W => S option" ("gn'_proj'_6/'(_')" [10] 999)
gn_projX8 :: "W => T option" ("gn'_proj'_7/'(_')" [10] 999)
gn_projX9 :: "W => U option" ("gn'_proj'_8/'(_')" [10] 999)

instance S:: type
by intro_classes
instance T:: type
by intro_classes
instance U:: type
by intro_classes
instance V:: type
by intro_classes
instance W:: type
by intro_classes

axioms
ga_embedding_injectivity [rule_format] :
"ALL x. ALL y. gn_inj(x) = gn_inj(y) --> x = y"

ga_projection_injectivity [rule_format] :
"ALL x. ALL y. exEqualOp (gn_proj(x), gn_proj(y)) --> x = y"

ga_projection [rule_format] : "ALL x. gn_proj(gn_inj(x)) = Some x"

ga_embedding_injectivity_1 [rule_format] :
"ALL x. ALL y. gn_inj_4(x) = gn_inj_4(y) --> x = y"

ga_projection_injectivity_1 [rule_format] :
"ALL x. ALL y. exEqualOp (gn_proj''(x), gn_proj''(y)) --> x = y"

ga_projection_1 [rule_format] :
"ALL x. gn_proj''(gn_inj_4(x)) = Some x"

ga_embedding_injectivity_2 [rule_format] :
"ALL x. ALL y. gn_inj_7(x) = gn_inj_7(y) --> x = y"

ga_projection_injectivity_2 [rule_format] :
"ALL x. ALL y. exEqualOp (gn_proj_5(x), gn_proj_5(y)) --> x = y"

ga_projection_2 [rule_format] :
"ALL x. gn_proj_5(gn_inj_7(x)) = Some x"

ga_embedding_injectivity_3 [rule_format] :
"ALL x. ALL y. gn_inj_9(x) = gn_inj_9(y) --> x = y"

ga_projection_injectivity_3 [rule_format] :
"ALL x. ALL y. exEqualOp (gn_proj_9(x), gn_proj_9(y)) --> x = y"

ga_projection_3 [rule_format] :
"ALL x. gn_proj_9(gn_inj_9(x)) = Some x"

ga_transitivity [rule_format] :
"ALL x. gn_inj_4(gn_inj(x)) = gn_inj'(x)"

ga_transitivity_1 [rule_format] :
"ALL x. gn_inj_7(gn_inj_4(x)) = gn_inj_5(x)"

ga_transitivity_2 [rule_format] :
"ALL x. gn_inj_9(gn_inj_7(x)) = gn_inj_8(x)"

end
