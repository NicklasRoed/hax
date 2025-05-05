(* File: extract_translations.v *)

(* Add the parent directory to the Coq load path *)
Add LoadPath "../" as TranslationsDir.

(* Import the extraction mechanism *)
Require Extraction.

(* Import the translations module (now using the added path) *)
Require Import translations.

(* Set extraction language *)
Extraction Language OCaml.

(* Add optimization directives with proper syntax *)
Extraction Optimize Inlining.
Extraction Optimize UnboxingIf.
Extraction Optimize ConstantPropagation.

(* Add custom extraction directives for better OCaml code *)
Extract Inductive bool => "bool" [ "true" "false" ].
Extract Inductive list => "list" [ "[]" "(::)" ].
Extract Inductive nat => "int" [ "0" "succ" ] 
  "(fun fO fS n -> if n=0 then fO () else fS (n-1))".
Extract Inductive Z => "int" [ "0" "(fun x -> x + 1)" "(fun x -> x - 1)" ]
  "(fun zero succ pred n -> if n=0 then zero () else if n>0 then succ (n-1) else pred (-n-1))".
Extract Inductive option => "option" [ "Some" "None" ].
Extract Inductive unit => "unit" [ "()" ].

(* Extract to OCaml file *)
Extraction "translations.ml" translations.