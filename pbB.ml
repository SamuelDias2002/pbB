type lerarvore = int Array
module type Ordenado = sig (*Modulo necessário para fazer comparação *)
type a
val comparar: a -> a -> int
end
module Make (Ord: Ordenado) = struct 
 

type a  = Empty | Node of a  * Ord.a * a  * int 




let altura = function
  | Empty -> 0
  | Node (_,_,_,h) -> h

let node e x d = Node (e,x,d, 1+max (altura e) (altura d))

let balance e x d = (* Equilibrar a avl *)
  let ae = altura e in
  let ad = altura d in
  if ae > ad + 1 then begin
    match e with
    | Node (ee, ex, ed, _) when altura ee >= altura ed -> node ee ex (node ed x e)
    | Node (ee, ex, Node (ede, edx , edd,_),_) -> node (node ee ex ede) edx (node edd x d)
    | _ -> assert false 
end 
else if ad > ae + 1 then begin
match d with
| Node (de, dx, dd, _) when altura dd >= altura de ->
node (node e x de) dx dd
| Node (Node(dee, dex, ded, _), dx, dd, _) ->
node (node d x dee) dex (node ded dx dd)
| _ ->
assert false
end 
else node e x d

(*Adicionar valor *)
let rec add valor a =
  match a with
  | Empty -> Node (Empty, valor, Empty, 1)
  | Node (e,x,d,_) as a -> let c = Ord.comparar valor x in
      if c = 0 then a else if c < 0 then balance (add valor e) x d else balance e x (add valor d)

      
(* Procurar valores em comum -> Falta fazer as listas para serem comparadas e pegar nos valores iguais e juntar ás listas*)
let procurar valor =
| Empty -> false
| Node  (e,x,d) when x = valor -> true
| Node  (e,_,d) -> procurar x e || procurar x d









end



(* Ler quantos nodos a arvore vai ter e pedir o valor dos nodos (???) *)
let a = Array.make 
let n = read_int () (* Le nr de nodos que a arvore vai ter *)
let ler_arvore a =

for i = 0 to n - 1 do
    let valores = Scanf.scanf "%d" (fun n -> n) in
    a.(i) <- valores
done;; (* Pede o valor de n mas não pede os valores para o array*)

