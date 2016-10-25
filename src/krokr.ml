module OrigBytes = Bytes
open StdLabels

type kind = Static | Root | CatchAll | Parameter [@@deriving show]

(* XXX: remove 'mutable' *)
(* XXX: path: string -> CCString.Sub.t*)
type node = {
    mutable path: string;
    mutable wild_child: bool;
    mutable kind: kind;
    mutable max_parameters: int;
    mutable indices: bytes;
    mutable children: node array;
    mutable handler: (unit -> unit) option (* XXX: change *);
    mutable priority: int
  } [@@deriving show]
let mk_node ?(path="") ?(wild_child=false)
            ?(kind=Static) ?(max_parameters=0)
            ?indices ?(children=[||])
            ?(handler=None) ?(priority=0) () =
  {path; wild_child; kind; max_parameters;
   indices = (match indices with Some i -> i | None -> Bytes.create 0);
   children; handler; priority}

let create () =
  mk_node ~kind:Root ()

let has_children node =
  CCArray.length node.children > 0

(* XXX: do we need this? *)
let number_of_parameters path =
  CCString.fold (fun acc char ->
      if (char = ':') || (char = '*')
      then acc + 1
      else acc) 0 path

let incr_priority ?(by=1) node =
  node.priority <- node.priority + by

let is_param = function
  | '*' | ':' -> true
  | _ -> false

let last_char s =
  String.get s (String.length s - 1)

let failf v = Printf.ksprintf (fun b -> failwith b) v

let common_prefix_index s1 s2 =
  let i = ref 0 in
  let max = min (String.length s1) (String.length s2) in
  while (!i < max) && s1.[!i] = s2.[!i] do
    incr i
  done;
  !i

let sub ~from ?end_ s =
  let end_' = match end_ with
    | Some e -> e
    | None -> String.length s in
  let num = end_' - from in
  String.sub s from num

let take_upto ~sub s =
  CCString.take (CCString.find ~sub s) s

let index_opt s c =
  try Some (Bytes.index s c)
  with Not_found -> None

let find_wildcard_end ~from ~max path =
  let rec aux pos =
    if pos < max
    then
      begin
        let c = path.[pos] in
        if c = '/'
        then Some pos
        else (if is_param c
              then None
              else aux (pos + 1))
      end
    else
      Some max
  in
  aux from

let insert_child node num_params path full_path handler =
  let max = String.length path in
  let rec aux n pos offset = function
    | rest_params when (rest_params > 0) && (is_param path.[pos]) ->
       let end_ = (match find_wildcard_end ~from:(pos+1) ~max path with
                   | Some e -> e
                   | None -> failf "Only one wildcard per path is allowed, has: '%s' in path '%s'"
                                   (sub ~from:pos path) full_path) in
       (* existing children would be unreachable if we insert wildcard here *)
       if has_children n
       then failf "wildcard route '%s' conflicts with existing children in path '%s'"
                  (sub ~from:pos ~end_ path) full_path;
       (* check for empty wild card *)
       if end_ - pos < 2
       then failf "wildcards must not be empty in path '%s'" full_path;
       if path.[pos] = ':'
       then (* Parameter *)
         begin
           let offset = if pos > 0
                        then (n.path <- sub ~from:offset ~end_:pos path;
                              pos)
                        else offset in
           let child = mk_node ~kind:Parameter ~priority:1
                               ~max_parameters:rest_params () in
           n.children <- [|child|];
           n.wild_child <- true;
           let rest_params = rest_params - 1 in
           (* if the path doesn't end with the wildcard, then there will
            we be another non-wildcard subpath starting with '/' *)
           if end_ < max
           then
             begin
               child.path <- sub ~from:offset ~end_ path;
               let child2 = mk_node ~max_parameters:rest_params
                                    ~priority:1 () in
               child.children <- [|child2|];
               aux child2 (pos+1) end_ rest_params
             end
           else
             aux child (pos+1) offset rest_params
         end
       else (* CatchAll *)
         begin
           if (end_ != max) || (rest_params > 1)
           then failf "catch-all routes are only allowed at the end of the path in path '%s'"
                      full_path;
           (* XXX: check logic *)
           if (String.length n.path > 0) && last_char n.path = '/'
           then failf "catch-all conflicts with existing handle for the path segment root in path '%s'"
                      full_path;
           let pos = pos - 1 in
           if pos > 0 || path.[pos] <> '/'
           then failf "no / before catch-all in path: '%s'" full_path;
           n.path <- sub ~from:offset ~end_:pos path;
           (* first node: CatchAll with empty path *)
           let child = mk_node ~wild_child:true ~priority:1
                               ~kind:CatchAll ~max_parameters:1 () in
           n.children <- [|child|];
           n.indices <- Bytes.make 1 path.[pos];
           (* second node: node holding the variable *)
           let child2 = mk_node ~path:(sub ~from:pos path)
                                ~kind:CatchAll ~max_parameters:1
                                ~handler:(Some handler)
                                ~priority:1 () in
           child.children <- [|child2|]
         end

    | rest_params when rest_params > 0 ->
       aux n (pos + 1) offset rest_params
    | _ ->
       n.path <- sub ~from:offset path;
       n.handler <- Some handler
  in aux node 0 0 num_params

let increment_child_priority node pos =
  let child = node.children.(pos) in
  incr_priority child;
  let prio = child.priority in
  let new_pos = ref pos in
  while (!new_pos > 0) && node.children.(!new_pos - 1).priority < prio do
    let np = !new_pos in
    let tmp = node.children.(np - 1) in
    node.children.(np - 1) <- node.children.(np);
    node.children.(np) <- tmp;
    decr new_pos
  done;
  let np = !new_pos in
  (* build new index char string *)
  if !new_pos <> pos
  then
    begin
      let c = Bytes.get node.indices pos in
      Bytes.blit ~src:node.indices ~src_pos:(np)
                  ~dst:node.indices ~dst_pos:(np + 1)
                  ~len:(pos - np);
      Bytes.set node.indices np c;
    end;
  np

let add_to_non_empty node full_path num_params handler =
  let rec aux n path rest_params =
    if rest_params > n.max_parameters then n.max_parameters <- rest_params;
    match common_prefix_index path n.path with
    | i when i < String.length n.path ->
       (* split edge *)
       let child = mk_node ~path:(sub ~from:i n.path)
                           ~wild_child:n.wild_child
                           ~kind:Static
                           ~indices:n.indices
                           ~children:n.children
                           ~handler:n.handler
                           ~priority:(n.priority - 1) () in
       Array.iter child.children
                  ~f:(fun c ->
                    if c.max_parameters > child.max_parameters
                    then child.max_parameters <- c.max_parameters);
       n.children <- [|child|];
       n.indices <- Bytes.make 1 n.path.[i];
       n.path <- sub ~from:0 ~end_:i path;
       n.handler <- None;
       n.wild_child <- false
    | i when i < String.length path ->
       (* make new node a child of this node *)
       let path = sub ~from:i path in
       if n.wild_child then
         begin
           let n = n.children.(0) in
           incr_priority n;
           if rest_params > n.max_parameters
           then n.max_parameters <- rest_params;
           let rest_params = rest_params - 1 in
           let npl = String.length n.path in
           let pl = String.length path in
           (* Check if the wildcard matches*)
           if (pl >= npl) && (CCString.prefix ~pre:n.path path)
              (* Check for longer wildcard, e.g. :name and :names *)
              && ((npl >= pl) || (path.[npl] = '/'))
           then
             aux n path rest_params (* recur *)
           else
             begin
               (* wildcard conflict *)
               let path_seg = fst @@ CCOpt.get_exn
                              @@ CCString.Split.left ~by:"/" path in
               let prefix = (take_upto ~sub:path_seg full_path) ^ n.path in
               failf "'%s' in new path '%s' conflicts with existing wildcard '%s' in exsiting prefix '%s'"
                     path_seg full_path n.path prefix
             end
         end;
       let c = path.[0] in
       (match n.kind, c, n.children, index_opt n.indices c with
        | Parameter, '/', [|child|], _->
           (* slash after parameter *)
           incr_priority child;
           aux child path rest_params
        | _, _, _, Some i ->
           (* child with the next path byte exists*)
           let i' = increment_child_priority n i in
           let n' = n.children.(i') in
           aux n' path rest_params
        | _ ->
           let n' =
             if is_param c then
               begin
                 (* XXX: bug in stdlib: no BytesLabels.cat, so we have
                    to use this hack *)
                 n.indices <- OrigBytes.cat n.indices (Bytes.make 1 c);
                 let child = mk_node ~max_parameters:rest_params () in
                 n.children <- Array.append n.children [| child |];
                 ignore @@ increment_child_priority
                            n (Bytes.length n.indices - 1);
                 child
               end
             else n in
           insert_child n' rest_params path full_path handler)
    | _ (* i when i = String.length path *) ->
       (* make node a (in-path) leaf *)
       (match n.handler with
        | Some _ -> failf "a handle is already registered for '%s'" full_path
        | None -> n.handler <- Some handler)
  in aux node full_path num_params

let add_route node path handler =
  let num_params = number_of_parameters path in
  let () = incr_priority node in
  if (String.length node.path > 0) || (has_children node)
  then add_to_non_empty node path num_params handler
  else (* empty tree *)
    (insert_child node num_params path path handler;
        node.kind <- Root)
