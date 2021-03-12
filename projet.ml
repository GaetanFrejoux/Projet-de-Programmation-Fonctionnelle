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
let x = string_to_token_list " 34 56 2 + x * -;";;


(*Analyse lexicale*)

(*Cette partie est donnée dans le sujet en utilisant le module expression_scanner*)






(*Analyse syntaxique et construction de l'arbre*)

(*TODO*)

(*fonctions*)

(*fonction qui transforme une liste de token en un arbre de syntaxe abstraite*)
let parse list =
  let rec parse_aux list stack =
    match list with
    | [] -> ()
    | hd::tl ->  ignore((match hd with
                 | Variable(v) -> push (Var(v)) stack
                                    
                 | Number(n) -> push (Cst(n)) stack
                              
                 | Minus -> let valeur = pop stack in
                            push (Unary(valeur)) stack
                            
                 | End -> ()
                 | _ -> let v1 = pop stack in
                        let v2 = pop stack in
                        match hd with
                        | Add -> push (Binary(Plus,v2,v1)) stack
                        | Subtract -> push (Binary(Minus,v2,v1)) stack
                        | Multiply -> push (Binary(Mult,v2,v1)) stack
                        | Divide -> push (Binary(Div,v2,v1)) stack
                   ));
                 parse_aux tl stack
  in
  let stack = create() in
  ignore((parse_aux list stack));
  pop stack
;;


let ans = parse x;;






(*Simplication sur l'arbre*)

(*TODO*)

(*fonction qui simplifie un arbre de syntaxe abstraite*)







(*Affichage du résultat*)

(*TODO*)

(*fonctionn qui transforme un arbre de syntaxe abstraite en un string*)



