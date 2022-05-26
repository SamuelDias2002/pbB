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
    | Node (e, x, d, _) -> if valor < x then procurar(valor)(e) else if valor > x then
      procurar(valor)(d) else true
   

let rec armazenacaminho arvore valor =
let rec accArmazenar arvore =
  match arvore with
  | Empty -> []
  | Node (e, x, d, _) ->
    if procurar(valor)(e) then x::accArmazenar (e) else x::accArmazenar (d) in ();

let caminho = accArmazenar arvore in caminho

let procuraMutacao arvore a b  = 
   
    let caminhoA = armazenacaminho arvore a in
    let caminhoB = armazenacaminho arvore b in
    let acc1 = ref 0 in
    let acc2 = ref 0 in
    let w = acc1 in
    let z = acc2 in
    let ultimoComum = ref 0 in  
      while (!w < List.length caminhoA) && (!w < List.length caminhoB) do
        let rec elemento p lista = 
        match p, lista with
        |_,[] -> failwith "Valor Inválido"
        |0, c::t -> c
        |p, c::t -> elemento (p-1) t
        
      in
      if elemento !w caminhoA = elemento !z caminhoB   then
        if (elemento !w caminhoA = a) || (elemento !z caminhoB = b) then
          let  () = 
          ultimoComum := elemento !w caminhoA;
          w := List.length caminhoA
        in ()
      else 
        let  () = 
          ultimoComum := elemento !w caminhoA ; incr w ; incr z in ()
        else
          w := List.length caminhoA
        done;
!ultimoComum 

let closestNode arr x a b arv =
  if procurar (a) (arv) && procurar (b) (arv) then arr.(x) <- procuraMutacao arv a b else arr.(x) <- (-1)


let v = ref 0 (*Variavél útil*)
let nrArvores = read_int ()
let () = if nrArvores <= 0 || nrArvores > 5000 then failwith "VALOR INVÁLIDO"

let hashtable = Hashtbl.create nrArvores (*Criar a floresta*)

let ()= 
for i = 0 to nrArvores - 1 do
  let nrElementos = read_int () in
  if nrElementos <= 0 || nrElementos > 10_000 then failwith "Número de elementos inválido"
  else 
    for j = 0 to nrElementos - 1 do
      let valor = read_int () in 
      let arvoreHTB = 
        if Hashtbl.mem hashtable i then Hashtbl.find hashtable i
        else
          Empty in Hashtbl.replace hashtable i (add valor arvoreHTB)
        done
      done

let (a,b) = Scanf.scanf "%d %d" (fun a b -> (a,b))


let () =
let arrayOutput = Array.make nrArvores 0 in
for k = 0 to nrArvores -1  do 
  let teste = Hashtbl.find hashtable k in 
  (*
  Lista pq funcao procuramutaçao agora é uma lista
     
  
  let nodomaisproximo = if a < raiz && b > raiz || a > raiz && b < raiz then raiz
      else if a = b then a
      else
        procuraproximo acaminho bcaminho in
  
  
  
  *)
  for l = 0 to nrArvores - 1 do 
    if arrayOutput.(l) = -1 then incr v
    done
  done;
  if !v = nrArvores then Printf.printf "NO\n" else
for s = 0 to nrArvores - 1 do 
      if arrayOutput.(s) = -1 then Printf.printf "" else Printf.printf "%d\n" arrayOutput.(s)
done