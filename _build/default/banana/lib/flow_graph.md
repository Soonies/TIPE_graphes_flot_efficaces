# Implementation of the `flow_graph.ml` module

## General description

We use two custom-made data-structures : *Dynamic Arrays* and *Linked arrays* along with the one implemented by the module *Hashtbl* from the standard library.

We use an implementation inspired by the foward and reverse star representation described in the book _Network algorithms_, which has been modified to allow for the suppression of edges while guarantying an access to the successors and predecessors of a vertex in O(1) time.

A graph is implemented as a record with multiple fields:

 1. mutable uU  :  int ;
    
  2. mutable cC : int  ; 
0.  `v_representant` 1 vertex amongst the other. No particular meaning. needed for the get_1_of function ( no other way to get a random vertex without iterating through the whole graph)
1. `v : ('a vertex, int array)` Dictionnary storing the attributes of the vertices
    * keys :  ` 'a vertex `
    * entry : `a : int array`.
    * Representation : `a` = `[|i ; b(i)|]` with `i` being the unique index of the vertex and `b(i)` the supply of the vertex 


2. ` e: ('a edge, int)` Dictionnary storing the unique index of the edges in the attribute array (see below)
    * keys :  `'a edges`
    * entries : `int` the unique index of the edge 

3. `edges : 'a edge_att_rec D.t` Dynamic_Array storing the attributes of the edge as a  `'a edge_att_rec =  {edge : 'a edge ; cntnt : cout ref *int array }` record
    * at the index `n` corresponding to an edge , the content of the record is as such: 

      ` ref |Infty |NInfty | C of int * [|capa;||||--cout--||||;bindx;findx; flow|]` (*justement on a un prblm, je veux mettre une capa infinie donc j'ai le choix du -1 mais il faudra tout modifier
      
      Changement a faire :  Ok -  infty capa  =  -1, c un pointeur a g.uU
                            - Infty cost :  + infini :  -1
                                            - infi : -2 ok?*)

4.  `fptr : int L.t` and `bptr : int L.t` // Linked arrays of the pointer-chains for the successors and predecessors of vertices (resp.) 

5. `frange: int array;`
  and `brange: int array;` // range of the successors and predecessors of a vetex in the pointer-chain (resp)

> 6.  `mutable f : 'a flow` //  function of type `('a edge -> int` attributing a flow value to a specific edge  |||||NO LONGER EXISTS
  

## Implementation of types

### `vset`
A set of vertices:
  _Can either be a `list` of vertices or a `FRange` refering to  a range in the foward pointer-chain, or a `BRange` refering to a range in the backqrd pointer-chain

  The user can create a `vset` from a list with the `vset` constructor : `vset_of_vertex_list`.
  The other type constructors are sealed and can only be used through the iterator  `iter_vset`

  The code is very straight foward

### `Capacity`
Can either be `Infty` of a `Capa` of some `int`

The code is very straight foward


## Functions descriptions

* `size` -> returns the number of entries of the `v H.t`
* `nb_edge` -> returns the number of entries of the `e H.t`
* `see_label` -> patter matches on the label of the vertex
* `see_supply` -> accesses to the supply field of the vertex 