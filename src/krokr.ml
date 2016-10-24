
type kind = Static | Root | CatchAll | Parameter [@@deriving show]

(* XXX: remove 'mutable' *)
(* XXX: path: string -> CCString.Sub.t*)
type node = {
    mutable path: string;
    mutable wild_child: bool;
    kind: kind;
    mutable max_parameters: int;
    mutable indices: string;
    mutable children: node list;
    mutable handler: (unit -> unit) option (* XXX: change *);
    mutable priority: int
  } [@@deriving show]
let mk_node ?(path="") ?(wild_child=false)
            ?(kind=Static) ?(max_parameters=0)
            ?(indices="") ?(children=[])
            ?(handler=None) ?(priority=0) () =
  {path; wild_child; kind; max_parameters; indices;
   children; handler; priority}

let create () =
  mk_node ~kind:Root ()

let has_children node =
  not @@ CCList.is_empty node.children

(* XXX: do we need this? *)
let number_of_parameters path =
  CCString.fold (fun acc char ->
      if (char = ':') || (char = '*')
      then acc + 1
      else acc) 0 path

let increment_priority ?(by=1) node =
  node.priority <- node.priority + by

let is_param = function
  | '*' | ':' -> true
  | _ -> false

let last_char s =
  String.get s (String.length s - 1)

let failf v = Printf.ksprintf (fun b -> failwith b) v

let sub ~from ?end_ s =
  let end_' = match end_ with
    | Some e -> e
    | None -> String.length s in
  let num = end_' - from in
  String.sub s from num

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
           n.children <- [child];
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
               child.children <- [child2];
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
           n.children <- [child];
           n.indices <- String.make 1 path.[pos];
           (* second node: node holding the variable *)
           let child2 = mk_node ~path:(sub ~from:pos path)
                                ~kind:CatchAll ~max_parameters:1
                                ~handler:(Some handler)
                                ~priority:1 () in
           child.children <- [child2]
         end

    | rest_params when rest_params > 0 ->
       aux n (pos + 1) offset rest_params
    | _ ->
       n.path <- sub ~from:offset path;
       n.handler <- Some handler
  in aux node 0 0 num_params


let add_route node path handler =
  let num_params = number_of_parameters path in
  let full_path = path in
  let () = increment_priority node in
  if (String.length node.path > 0) || (has_children node)
  then failwith "not implemented"
  else insert_child node num_params path full_path handler
