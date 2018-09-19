module ListTreeHeap

open System.Reflection.Metadata
open System.Net.Http.Headers

//binary heap node
type node<'T> = {
    head: 'T
    left: option<node<'T>>
    right: option<node<'T>>
}

//was originally going to be implemented as a list.
//actually record-node tree.
//note that node may be a tree of size > than size; in that case all nodes past node size should be ignored.
type listHeap<'T> = {
    size : uint32
    heap : node<'T> option
}

(* refactor section below to be internal to getBranch *)

//constants for 1 at leftmost digit and rightmost digit respectively with all else zero.
let lint = 0x80000000u
let rint = 0x00000001u

//shifts section of int starting with highest nonzero bit to start at the highest bit of int.
let rec lwind index = 
    if index >= lint then index else lwind (index <<< 1)

//shifts bits to left with an additional 1 bit at lower end of the original bits, then removes the highest bit.
let lwindandflag index =
    (lwind ((index <<< 1) + 1u)) <<< 1

//gets subtree on heap headed by node at pos
let getBranch heap pos =
    let rec getWithSel heap sel =
        if sel = lint then heap else
            match heap with
            | Some n -> if sel < lint then getWithSel n.left (sel <<< 1) else getWithSel n.right (sel <<< 1)
            | None -> None
    getWithSel heap (lwindandflag pos)
    
// places subheap dheap on uheap at position pos. sifts up to retain heapiness.
let rec addHeap uheap pos dheap = 
    if pos = 1u then Some dheap else
    let pos2 = pos >>> 1
    let uno = getBranch uheap pos2
    match uno with
    | Some un -> addHeap uheap pos2 (
                    let (|Even|Odd|) n = if (n &&& rint) = 0u then Even else Odd
                    if dheap.head < un.head then
                        match pos with
                        |Even -> {
                                    head = dheap.head
                                    left = Some {head = un.head; left = dheap.left; right = dheap.right} 
                                    right = un.right
                                 }
                        |Odd -> {
                                    head = dheap.head
                                    left = un.left
                                    right = Some {head = un.head; left = dheap.left; right = dheap.right}
                                }
                    else
                        match pos with 
                        |Even -> {
                                    head = un.head
                                    left = Some dheap
                                    right = un.right
                                 }
                        |Odd -> {
                                    head = un.head
                                    left = un.left
                                    right = Some dheap
                                }
                         
                 )
    | None -> None

// adds item n to lheap.
let add lheap n =
    let pos = lheap.size + 1u
    {heap = addHeap lheap.heap pos {head = n; left = None; right = None}; size = pos}

(* the below section is used to calculate the sizes of the left and right subheaps given the size of the super-heap. 
   messy bit manipulation sorry *)

let rec rHighestOne n i = 
    if i = 1u then n else rHighestOne (n<<<1) (i>>>1)

let highestOne = rHighestOne 1u

let hAllOnes n = highestOne (n + 1u) - 1u

//left subheap
let lsize s = 
    let b = hAllOnes s
    let r = b >>> 1
    let l = s - r - 1u
    if l > b then b else l

//right subheap
let rsize s = 
    let b = hAllOnes s
    let r = b >>> 1
    if s > b + r + 1u then s - b - 1u else r

//replaces root of lheap with n; sifts down to retain heapiness.
let rec replace lheap n = 
    match lheap with
    | {size = 1u; heap = Some h} -> Some (h.head, {size = 1u; heap = Some {head = n; left = None; right = None}})
    | {size = 2u; heap = Some {head = ohead; left = Some oleft; right = _}} -> if oleft.head < n then
                                                                                match replace {size = 1u; heap = Some oleft} n with 
                                                                                | Some (nhead, nleft) -> Some (ohead, {size = 2u; heap = Some {head = nhead; left = nleft.heap; right = None}})
                                                                                | None -> None
                                                                               else Some (ohead, {size = 2u; heap = Some {head = n; left = Some oleft; right = None}})
    | {size = sz; heap = Some {head = ohead; left = Some oleft; right = Some oright}} when sz > 2u -> if oright.head <= oleft.head && oright.head < n then
                                                                                                        match replace {size = rsize sz; heap = Some oright} n with
                                                                                                        | Some (nhead, nright) -> Some(ohead, {size = sz; heap = Some {head = nhead; left = Some oleft; right = nright.heap}})
                                                                                                        | None -> None
                                                                                                      elif oleft.head < n then
                                                                                                        match replace {size = lsize sz; heap = Some oleft} n with
                                                                                                        | Some (nhead, nleft) -> Some(ohead, {size = sz; heap = Some {head = nhead; left = nleft.heap; right = Some oright}})
                                                                                                        | None -> None
                                                                                                      else Some (ohead, {size = sz; heap = Some {head = n; left = Some oleft; right = Some oright}})
    | u -> None

//pops the root of lheap; rebalances and sifts down to retain heapiness.
let pop lheap = 
    match getBranch lheap.heap lheap.size with
    | Some {head = n; left = _; right = _;} -> match replace lheap n with
                                               | Some (v, h) -> Some (v, {size = h.size-1u; heap = h.heap})
                                               | None -> None
    | None -> None
(* list to heap and heap to list *)
let rec heapify lth l =
    match l with 
    | i :: r -> heapify (add lth i) r
    | [] -> lth

let rec deheapify lth l = 
    match pop lth with
    | Some (n, nl) -> deheapify nl (n :: l) 
    | None -> l 

//heapsort from largest to smallest.
let heapsort l = 
    deheapify (heapify {size = 0u; heap = None} l) []