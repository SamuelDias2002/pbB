type 'a avl  =  Empty | Node of 'a avl  * 'a * 'a avl  * int 




let altura = function
  | Empty -> 0
  | Node (_,_,_,h) -> h

let node e x d = Node (e,x,d, 1+max (altura e) (altura d))

let balance e x d = (* Equilibrar a avl *)
  let ae = altura e in
  let ad = altura d in
  if ae > ad + 1 then begin
    match e with
    | Node (ee, ex, ed, _) when altura ee >= altura ed -> node ee ex (node ed x d)
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


let rec add valor = function
  | Empty -> Node (Empty, valor, Empty, 1)
  | Node (e,x,d,_) as a -> let c = compare  valor x in
  if c = 0 then a
  else if c < 0 then balance (add valor e) x d 
  else balance e x (add valor d);;


let rec procurar valor arvore =
    match arvore with
    | Empty -> false
    | Node (e, x, d, _) when x = valor -> true
    | Node (e, _, d, _) -> procurar(valor)(e) || procurar(valor)(d);;

let rec armazenacaminho arvore valor =
let rec accArmazenar arvore =
  match arvore with
  | Empty -> []
  | Node (e, x, d, _) ->
    if procurar(valor)(e) then x::accArmazenar (e) else x::accArmazenar (d) in ();

let caminho = accArmazenar arvore in caminho

let procuraMutacao arvore a b =
    let caminhoA = armazenacaminho arvore a in
    let caminhoB = armazenacaminho arvore b in
    let ultimoComum = ref 0 in  
    let (w,z) = (ref 0, ref 0) in
    let  () =
      while !w < List.length(caminhoA) && !z < List.length(caminhoB) do
        if List.nth(caminhoA)(!w) == List.nth(caminhoB)(!z) then
          ultimoComum := List.nth(caminhoA)(!w); incr w; incr z;
      done;
    in !ultimoComum 



  let leitura ()=
  let nrArvores = read_int () in
  let listaArvores = ref [] in (* Pede nr de arvores *)
  if nrArvores  <= 0 || nrArvores > 5000 then failwith "Número de árvores Inválido" else
   (*Mutavel pq pode armazenar valores*) 
  for i = 0 to nrArvores-1 do                   
    let nodos = ref [] in (*Nodos da arvores*)
    let qtdNodos = read_int () in (*Quantidade de elementos que a arvore tem*)
    let valorNodos = read_int () in (*Valor dos Nodos*)
    if valorNodos <= 0 || valorNodos > 10000 then failwith "Valores inválidos"
    else 
    for j = 0 to qtdNodos - 1 do 
      nodos := !nodos@[valorNodos]
    done;
     listaArvores := !listaArvores@[!nodos]
  done;;

  let (a, b) = Scanf.scanf "%d %d" (fun a b -> (a, b))

