open Base
open Pyops

module Ax = struct
  type t = Py.Object.t

  let set_title t title = ignore (t.&("set_title") [| Py.String.of_string title |])

  let set_xlim t ?left ?right () =
    let left =
      match left with
      | Some left -> Py.Float.of_float left
      | None -> Py.none
    in
    let right =
      match right with
      | Some right -> Py.Float.of_float right
      | None -> Py.none
    in
    ignore (t.&("set_xlim") [| left; right |])

  let set_ylim t ~bottom ~top =
    ignore (t.&("set_ylim") [| Py.Float.of_float bottom; Py.Float.of_float top |])

  let set_xlabel t ?fontsize label =
    let keywords =
      List.filter_opt
        [ Option.map fontsize ~f:(fun f -> "fontsize", Py.Float.of_float f) ]
    in
    let args = [| Py.String.of_string label |] in
    ignore (Py.Module.get_function_with_keywords t "set_xlabel" args keywords)

  let set_ylabel t ?fontsize label =
    let keywords =
      List.filter_opt
        [ Option.map fontsize ~f:(fun f -> "fontsize", Py.Float.of_float f) ]
    in
    let args = [| Py.String.of_string label |] in
    ignore (Py.Module.get_function_with_keywords t "set_ylabel" args keywords)

  let set_aspect t ~aspect =
    let aspect =
      match aspect with
      | `auto -> Py.String.of_string "auto"
      | `equal -> Py.String.of_string "equal"
      | `f f -> Py.Float.of_float f
    in
    ignore (t.&("set_aspect") [| aspect |])

  let set_xticks t ?labels ticks =
    let keywords =
      List.filter_opt
        [ Option.map labels ~f:(fun l ->
            "labels", Py.List.of_array_map Py.String.of_string l)
        ]
    in
    let args = [| Py.List.of_array_map Py.Float.of_float ticks |] in
    ignore (Py.Module.get_function_with_keywords t "set_xticks" args keywords)

  let grid t ?which ?axis b =
    let keywords =
      let b = Some ("b", Py.Bool.of_bool b) in
      let which =
        Option.map which ~f:(fun which ->
          let which =
            match which with
            | `major -> "major"
            | `minor -> "minor"
            | `both -> "both"
          in
          "which", Py.String.of_string which)
      in
      let axis =
        Option.map axis ~f:(fun axis ->
          let axis =
            match axis with
            | `both -> "both"
            | `x -> "x"
            | `y -> "y"
          in
          "axis", Py.String.of_string axis)
      in
      List.filter_opt [ b; which; axis ]
    in
    ignore (Py.Module.get_function_with_keywords t "grid" [||] keywords)

  let plot = Mpl.plot
  let hist = Mpl.hist
  let scatter = Mpl.scatter
  let imshow = Mpl.imshow
  let legend = Mpl.legend

  module Expert = struct
    let to_pyobject = Fn.id
  end
end

module Ax3d = struct
  type t = Py.Object.t

  let set_title = Ax.set_title
  let set_xlim t ~left ~right = Ax.set_xlim t ~left ~right ()
  let set_ylim = Ax.set_ylim
  let set_xlabel = Ax.set_xlabel ?fontsize:None
  let set_ylabel = Ax.set_ylabel ?fontsize:None

  let set_zlim t ~bottom ~top =
    ignore (t.&("set_zlim") [| Py.Float.of_float bottom; Py.Float.of_float top |])

  let set_zlabel t label = ignore (t.&("set_zlabel") [| Py.String.of_string label |])

  let grid t b =
    let keywords = [ "b", Py.Bool.of_bool b ] in
    ignore (Py.Module.get_function_with_keywords t "grid" [||] keywords)

  let scatter = Mpl.scatter_3d
  let imshow = Mpl.imshow

  module Expert = struct
    let to_pyobject = Fn.id
  end
end

module Fig = struct
  type t = Py.Object.t

  let create ?figsize () =
    let p = Mpl.pyplot_module () in
    let keywords =
      let figsize =
        Option.map figsize ~f:(fun (w, h) ->
          "figsize", Py.Tuple.of_pair Py.Float.(of_float w, of_float h))
      in
      List.filter_opt [ figsize ]
    in
    Py.Module.get_function_with_keywords p "figure" [||] keywords

  let add_subplot t ~nrows ~ncols ~index =
    let keywords = [] in
    let args = [| nrows; ncols; index |] |> Array.map ~f:Py.Int.of_int in
    Py.Module.get_function_with_keywords t "add_subplot" args keywords

  let add_subplot_3d t ~nrows ~ncols ~index =
    let keywords = [ "projection", Py.String.of_string "3d" ] in
    let args = [| nrows; ncols; index |] |> Array.map ~f:Py.Int.of_int in
    Py.Module.get_function_with_keywords t "add_subplot" args keywords

  let create_with_ax ?figsize () =
    let t = create ?figsize () in
    let ax = add_subplot t ~nrows:1 ~ncols:1 ~index:1 in
    t, ax

  let create_with_two_axes ?figsize orientation =
    let t = create ?figsize () in
    let nrows, ncols =
      match orientation with
      | `horizontal -> 1, 2
      | `vertical -> 2, 1
    in
    let ax1 = add_subplot t ~nrows ~ncols ~index:1 in
    let ax2 = add_subplot t ~nrows ~ncols ~index:2 in
    t, ax1, ax2

  let suptitle t title = ignore (t.&("suptitle") [| Py.String.of_string title |])

  let set_tight_layout t tight =
    ignore (t.&("set_tight_layout") [| Py.Bool.of_bool tight |])

  let set_size_inches t ~w ~h =
    ignore (t.&("set_size_inches") [| Py.Float.of_float w; Py.Float.of_float h |])

  module Expert = struct
    let to_pyobject = Fn.id
  end
end
