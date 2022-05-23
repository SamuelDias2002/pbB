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


let v = ref 0
let nrArvores =  read_int () 
    let listaArvores = ref []
    let introducaovalores =
       (* Pede nr de arvores *)
      if nrArvores  <= 0 || nrArvores > 5000 then failwith "Número de árvores Inválido"
      else
      for i = 0 to nrArvores-1 do
        let listaNodos = ref [] in
          let qtdElementos = read_int () in
          if qtdElementos <= 0 || qtdElementos > 10000 then failwith "Valores inválidos"
          else
          for j = 0 to qtdElementos-1 do
              let valorNodos = read_int () in
              if valorNodos < 0 then failwith "Valor Negativo"
              else listaNodos:= !listaNodos@[valorNodos]
          done;
          listaArvores := !listaArvores@[!listaNodos]
      done;;
      let (a, b) = Scanf.scanf "%d %d" (fun a b -> (a,b))

let () =
for l = 0 to nrArvores-1 do
  let listaArvoresNodos = List.nth(!listaArvores)(l) in
  let criarNodo = node Empty (List.nth(listaArvoresNodos)(0)) Empty in
  let armazenaNodos = ref [criarNodo] in
  for k = 1 to List.length (listaArvoresNodos)-1 do
  armazenaNodos := add(List.nth(listaArvoresNodos)(k))(List.nth(!armazenaNodos)(0))::!armazenaNodos
  done;
  let arvoreFinal = List.nth(!armazenaNodos)(0) in



  let nodoProximo ()= 
  for i = 0 to nrArvores - 1 do
    let teste = if procurar(a)(arvoreFinal) && procurar(b)(arvoreFinal) then
       procuraMutacao (arvoreFinal) (a)(b) else -1
    in
    if teste = -1 then incr v ;
    if nrArvores = !v then Prinf.printf "NO\n" else Printf.printf "%d\n" teste
  done
done