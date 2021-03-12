(*load*)
#load "expression_scanner.cmo";;

(*open*)
open Expression_scanner;;
open Stack;;

(*show*)
#show Expression_scanner;;
#show Stack;;





(*Types*)

(*operator*)
type operator = | Plus | Minus | Mult | Div;;

(*tree*)
type tree =
  | Var of char
  | Cst of int
  | Unary of tree
  | Binary of operator * tree * tree
;;




(*tests*)
let x = string_to_token_list " 4 5 +;";;






(*Analyse lexicale*)

(*Cette partie est donnée dans le sujet en utilisant le module expression_scanner*)






(*Analyse syntaxique et construction de l'arbre*)

(*TODO*)

(*fonctions*)

(*fonction qui transforme une liste de token en un arbre de syntaxe abstraite*)
let token_list_to_abstract_syntax_tree list =
  let rec t_l_t_a_s_t_aux list tree =
    match list with
    | [] -> tree
    | hd::tl -> (*TODO*)
;;





(*Simplication sur l'arbre*)

(*TODO*)

(*fonction qui simplifie un arbre de syntaxe abstraite*)







(*Affichage du résultat*)

(*TODO*)

(*fonctionn qui transforme un arbre de syntaxe abstraite en un string*)




