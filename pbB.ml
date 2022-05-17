type a  = Empty | Node of 'a  * 'a * 'a  * int 




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
  | Node (e,x,d,_) as a -> let c = if valor = x then c = 0 else  if c < 0 then balance (add valor e) x d else balance e x (add valor d);

      
(* Procurar valores em comum -> Falta fazer as listas para serem comparadas e pegar nos valores iguais e juntar ás listas
let procurar valor =
| Empty -> false
| Node  (e,x,d) when x = valor -> true
| Node  (e,_,d) -> procurar x e || procurar x d
*)


let introducaovalores () =
  let nrArvores = read_int () in (* Pede nr de arvores *)
  if nrArvores  <= 0 || nrArvores > 5000 then failwith "Número de árvores Inválido"
  else
  let c = Array.make nrArvores Empty in
  for i = 0 to nrArvores-1 do 
    let qtdElementos = read_int () in
      if qtdElementos <= 0 || qtdElementos > 5000 then failwith "Valores inválidos"
      else 
      for j = 0 to qtdElementos - 1 do
          let valorNodos = read_int () in 
          if valorNodos < 0 then failwith "Valor Negativo"
          else
             add valorNodos a
      done
      c.(i) <- arvore
      
  done


(* --------------------------------------------------------- Input ---------------------------------------------------------*)

(* Ler quantas arvores vai ter + Pedir o número de elementos que a arvores vai ter + Pedir valor dos nodos
   Exemplo: nrArvores = 3
            -Array de 3 posições
            qtdElementos = 5
            -Arvore 1 de 5 elementos
            valorNodos = 2,3,4,5,6
*)