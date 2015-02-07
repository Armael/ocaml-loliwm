open Batteries
open Prelude

let active = ref None
let cut = ref 0.5

let views_of_space_h : (Wlc.Space.t, Wlc.View.t Dlist.t option) Hashtbl.t =
  Hashtbl.create 37

let views_of_space s =
  try Hashtbl.find views_of_space_h s with
    Not_found ->
    Hashtbl.add views_of_space_h s None;
    None

let set_views_of_space = Hashtbl.replace views_of_space_h

let remove_view space view_node =
  try
    let head_view_node = views_of_space space |> Option.get in
    if head_view_node == view_node then (
      let next = Dlist.next view_node in
      Dlist.remove view_node;
      set_views_of_space space (Some next)
    ) else (
      Dlist.remove view_node;
    )
  with
    Invalid_argument _
  | Dlist.Empty -> set_views_of_space space None

let ( **. ) i f = (Float.of_int i) *. f |> Float.to_int

let is_bemenu v = Wlc.View.get_class v = Some "bemenu"

let is_or v =
  List.mem Wlc.View.Override_Redirect (Wlc.View.get_type v)
  || is_bemenu v

let is_managed v =
  let typ = Wlc.View.get_type v in
  not (List.mem Wlc.View.Unmanaged typ
       && not (List.mem Wlc.View.Popup typ)
       && not (List.mem Wlc.View.Splash typ))

let is_modal v =
  List.mem Wlc.View.Modal (Wlc.View.get_type v)

let is_tiled v =
  let st = Wlc.View.get_state v in
  not (List.mem Wlc.View.Fullscreen st)
  && Wlc.View.get_parent v = None
  && is_managed v
  && not (is_or v)
  && not (is_modal v)

let layout_parent view parent size =
  (* Size to fit the undermost parent *)
  (* TODO: Use surface height as base instead of current *)
  let rec find_ancestor v = match Wlc.View.get_parent v with
    | None -> v
    | Some p -> find_ancestor p in
  let under = find_ancestor parent in

  let u = Wlc.View.get_geometry under in
  let p = Wlc.View.get_geometry parent in

  let cw = max size.Wlc.w (size.Wlc.w **. 0.6) in
  let ch = max size.Wlc.h (size.Wlc.h **. 0.6) in

  let size = Wlc.{ w = min cw (u.size.w **. 0.8);
                   h = min ch (u.size.h **. 0.8) } in
  let origin = Wlc.{ x = (p.size.w / 2) - (size.w / 2);
                     y = (p.size.h / 2) - (size.h / 2) } in
  Wlc.View.set_geometry view Wlc.{ size; origin }

let relayout space =
  let open Wlc in
  let res = Wlc.Output.of_space space
            |> Wlc.Output.get_resolution in

  let views = views_of_space space in
  let count = Dlist.enum_opt views |> Enum.filter is_tiled |> Enum.count in

 let height = res.h / (if count > 1 then count - 1 else 1) in
  let fheight = if res.h > height * (count - 1) then
      height + (res.h - height * (count - 1))
    else height in

  Dlist.fold_left_opt (fun (toggle, y) v ->
    if List.mem Wlc.View.Fullscreen (Wlc.View.get_state v) then
      Wlc.View.set_geometry v { origin = { x = 0; y = 0 };
                                size = res };

    if List.mem Wlc.View.Splash (Wlc.View.get_type v) then (
      let g = Wlc.View.get_geometry v in
      Wlc.View.set_geometry v
        { g with
          origin = { x = (res.w / 2) - (g.size.w / 2);
                     y = (res.h / 2) - (g.size.h / 2) }}
    );

    begin match Wlc.View.get_parent v with
      | Some parent when
          is_managed v &&
          not (is_or v) ->
        layout_parent v parent (Wlc.View.get_geometry v).size
      | _ -> () end;

    if not (is_tiled v) then
      (toggle, y)
    else (
      let slave = res.w **. !cut in
      Wlc.View.set_state v Wlc.View.Maximized true;

      Wlc.View.set_geometry v {
        origin = { x = if toggle then res.w - slave else 0; y };
        size = { w = if count > 1 then
                     if toggle then slave else res.w - slave
                   else res.w;
                 h = if toggle then
                     if y = 0 then fheight else height
                   else res.h }
      };

      (true, y + (if toggle then (if y = 0 then fheight else height) else 0))
    )
  ) (false, 0) views
  |> ignore

let spawn bin =
  if Unix.fork () = 0 then (
    Unix.setsid () |> ignore;
    Unix.execv "/bin/sh" [| "/bin/sh"; "-c"; bin |]
  )

let screenshot output =
  Wlc.log "not implemented"

let should_focus_on_create view =
  (* Do not allow unmanaged views to steal focus (tooltips, dnds, etc..) *)
  (* Do not allow parented windows to steal focus, if current window wasn't parent. *)
  let parent = Wlc.View.get_parent view in
  let typ = Wlc.View.get_type view in
  not (List.mem Wlc.View.Unmanaged typ
       && Option.is_some !active
       && Option.is_some parent
       && parent = !active)

let cycle comp =
  let space = Wlc.Compositor.get_focused_space comp in
  let views = views_of_space space in
  match views with
  | Some views ->
    let count = Dlist.enum views |> Enum.filter is_tiled |> Enum.count in
    (* Check that we have at least two tiled views so we don't get in infinite
       loop *)
    if count > 1 then (
      (* cycle until we hit the next tiled view *)
      let rec loop l =
        if not (is_tiled (Dlist.get l)) then
          loop (Dlist.prev l)
        else l
      in
      set_views_of_space space (Some (loop (Dlist.prev views)));
      relayout space
    )
  | None -> ()

let rec raise_all view =
  (* Raise view and all related views to top honoring the stacking order *)
  begin match Wlc.View.get_parent view with
  | Some parent ->
    raise_all parent;

    Wlc.View.get_space view
    |> Wlc.View.all_of_space
    |> List.filter (fun v -> v <> view && Wlc.View.get_parent v = Some parent)
    |> List.iter Wlc.View.bring_to_front
  | None -> ()
  end;
  Wlc.View.bring_to_front view

let rec set_active comp view_opt =
  if not (!active = view_opt) then (
    (* Bemenu should always have focus when open *)
    if Option.map_default is_bemenu false !active then
      Wlc.View.bring_to_front (Option.get !active)
    else (
      begin match view_opt with
        | Some view ->
          let views = Wlc.View.get_space view
                      |> Wlc.View.all_of_space
                      |> List.rev in

          (* if view has a child, focus it instead (recursively) *)
          (try set_active
                 comp
                 (Some (List.find (fun v -> Wlc.View.get_parent v = Some view) views))
           with Not_found -> ());

          (* only raise fullscreen views when focused view is managed *)
          if is_managed view && not (is_or view) then (
            try Wlc.View.bring_to_front
                  (List.find (Wlc.View.get_state %>
                              List.mem Wlc.View.Fullscreen)
                     views)
            with Not_found -> ()
          );

          (* Only set active for current view to false, if new view is on same
             output and the new view is managed *)
          begin match !active with
            | Some a when
                is_managed view &&
                Wlc.Output.of_space (Wlc.View.get_space a) =
                Wlc.Output.of_space (Wlc.View.get_space view) ->
              Wlc.View.set_state a Wlc.View.Activated false
            | _ -> () end;

          Wlc.View.set_state view Wlc.View.Activated true;
          raise_all view;

          (* Always bring bemenu to front when exists *)
          (try Wlc.View.bring_to_front (List.find is_bemenu views)
           with Not_found -> ());
        | None -> ()
      end;

      Wlc.Compositor.focus_view comp view_opt;
      active := view_opt
    )
  )

let active_space comp space =
  let views = Wlc.View.all_of_space space in
  if views = [] then
    set_active comp None
  else
    set_active comp (Some (List.last views))

let focus_space comp id =
  let output = Wlc.Compositor.get_focused_output comp in
  try
    Wlc.Output.get_spaces output
    |> flip List.nth id
    |> Wlc.Output.focus_space output
  with Failure _ -> ()

let move_to_output comp view id =
  try
    let o = Wlc.Compositor.get_outputs comp
            |> flip List.nth id in
    Wlc.View.set_space view (Wlc.Output.get_active_space o);
    Wlc.Compositor.focus_output comp o
  with Failure _ -> ()

let move_to_space comp view id =
  let active = Wlc.Compositor.get_focused_space comp in
  try
    Wlc.Output.get_spaces (Wlc.Output.of_space active)
    |> flip List.nth id
    |> Wlc.View.set_space view
  with Failure _ -> ()

let focus_next_or_previous_output comp direction =
  let active = Wlc.Compositor.get_focused_output comp in
  Wlc.Compositor.get_outputs comp
  |> Dllist.of_list
  |> Dllist.find ((=) active)
  |> (if direction then Dllist.next else Dllist.prev)
  |> Dllist.get
  |> Wlc.Compositor.focus_output comp

let focus_next_or_previous_view comp view direction =
  match views_of_space (Wlc.View.get_space view) with
  | Some views ->
    Dlist.find ((=) view) views
    |> (if direction then Dlist.next else Dlist.prev)
    |> Dlist.get
    |> Option.some
    |> set_active comp
  | None -> ()

let view_created comp view space =
  let views = views_of_space space in

  begin match !active with
  | Some v when is_bemenu view && is_bemenu v ->
    false
  | _ ->
    begin match views with
      | None -> set_views_of_space space (Some (Dlist.create view))
      | Some views_head ->
        Dlist.prepend views_head view |> ignore
    end;

    if should_focus_on_create view then
      set_active comp (Some view);

    relayout space;
    Wlc.log "new view";
    true
  end

let view_destroyed comp view =
  let space = Wlc.View.get_space view in
  let view_node = Dlist.get_node
      (views_of_space space |> Option.get)
      view
  in

  if !active = Some view then (
    active := None;

    match Wlc.View.get_parent view with
    | Some parent ->
      (* Focus the parent view, if there was one *)
      (* Set parent to None befor this to avoid focusing back to the dying
         view *)
      remove_view space view_node;
      Wlc.View.set_parent view None;
      set_active comp (Some parent)
    | None ->
      (* Otherwise focus previous one (stacking order) *)
      let prev = Dlist.prev view_node in
      if prev != view_node then (
        set_active comp (Some (Dlist.get prev))
      );
      remove_view space view_node
  );
  relayout space

let view_switch_space comp view space_from space_to =
  let view_node = Dlist.get_node
      (views_of_space space_from |> Option.get)
      view
  in

  remove_view space_from view_node;
  relayout space_from;
  view_created comp view space_to |> ignore;

  if Wlc.Output.(of_space space_from = of_space space_to) then
    active_space comp space_from
  else
    Wlc.View.set_state
      (List.last (Wlc.View.all_of_space space_from))
      Wlc.View.Activated
      true;

  if Wlc.Output.(get_active_space (of_space space_to) = space_to) then
    Wlc.View.all_of_space space_from
    |> List.rev
    |> List.iter (fun v ->
      if Some v <> !active then
        Wlc.View.set_state v Wlc.View.Activated false
    )

let view_geometry_request comp view geometry =
  let typ = Wlc.View.get_type view in
  let state = Wlc.View.get_state view in
  let tiled = is_tiled view in
  let action = (List.mem Wlc.View.Resizing state)
               || (List.mem Wlc.View.Moving state)
  in

  with_ret (fun return ->
    if tiled && not action then
      return ();

    if tiled then
      Wlc.View.set_state view Wlc.View.Maximized false;

    if List.mem Wlc.View.Fullscreen state ||
       List.mem Wlc.View.Splash typ then
      return ();

    match Wlc.View.get_parent view with
    | Some parent when
        is_managed view &&
        not (is_or view) ->
      layout_parent view parent geometry.Wlc.size
    | _ ->
      Wlc.View.set_geometry view geometry
  )

let view_state_request comp view state toggle =
  Wlc.View.set_state view state toggle;

  match state with
  | Wlc.View.Maximized ->
    if toggle then
      relayout (Wlc.View.get_space view)
  | Wlc.View.Fullscreen ->
    relayout (Wlc.View.get_space view)
  | _ -> ()

let keyboard_key comp view time modifiers key sym state =
  if modifiers.Wlc.mods = [Config.modifier] &&
     state = Wlc.Key_State_Pressed then (
    if sym = Config.exit_key then (
      Wlc.terminate (); false
    ) else if Option.is_some view && sym = Config.close_focus_key then (
      Wlc.View.close (Option.get view); false
    ) else if sym = Config.term_open_key then (
      spawn Config.term; false
    ) else if sym = Config.menu_open_key then (
      spawn Config.menu_app; false
    ) else if Option.is_some view && sym = Config.toggle_fullscreen_key then (
      let v = Option.get view in
      Wlc.View.set_state v Wlc.View.Fullscreen
        (not (List.mem Wlc.View.Fullscreen (Wlc.View.get_state v)));
      relayout (Wlc.Compositor.get_focused_space comp);
      false
    ) else if sym = Config.cycle_client_key then (
      cycle comp; false
    ) else if sym >= Wlc.Keysym._0 && sym <= Wlc.Keysym._9 then (
      focus_space comp
        (if sym = Wlc.Keysym._0 then 9 else sym - Wlc.Keysym._1);
      false
    ) else if sym = Config.nmaster_expand_key ||
              sym = Config.nmaster_shrink_key then (
      cut := !cut +. (if sym = Config.nmaster_shrink_key then -0.01 else 0.01);
      if !cut > 1. then cut := 1.;
      if !cut < 0. then cut := 0.;
      relayout (Wlc.Compositor.get_focused_space comp);
      false
    ) else if Option.is_some view &&
              (sym = Config.move_focus_output_one ||
               sym = Config.move_focus_output_two ||
               sym = Config.move_focus_output_three) then (
      move_to_output comp (Option.get view)
        (if sym = Config.move_focus_output_one then 0
         else if sym = Config.move_focus_output_two then 1
         else 2);
      false
    ) else if Option.is_some view &&
              sym >= Wlc.Keysym._F1 &&
              sym <= Wlc.Keysym._F10 then (
      move_to_space comp (Option.get view) (sym - Wlc.Keysym._F1);
      false
    ) else if sym = Config.rotate_output_focus_key then (
      focus_next_or_previous_output comp true; false
    ) else if Option.is_some view &&
              (sym = Config.move_client_focus_left ||
               sym = Config.move_client_focus_right) then (
      focus_next_or_previous_view comp (Option.get view)
        (sym = Config.move_client_focus_left);
      false
    ) else if sym = Config.screenshot_key then (
      screenshot (Wlc.Compositor.get_focused_output comp); false
    ) else true
  ) else true

let pointer_button comp view time modifiers button state =
  if state = Wlc.Button_State_Pressed then
    set_active comp (Some view);
  true

let output_created comp output =
  (* add some spaces *)
  for i = 0 to 9 do
    Wlc.Output.add_space output |> ignore;
  done;
  true

let resolution_notify comp output res =
  relayout (Wlc.Output.get_active_space output)

let output_notify comp output =
  active_space comp (Wlc.Output.get_active_space output)

let interface = Wlc.Interface.{
  view = {
    created = view_created;
    destroyed = view_destroyed;
    switch_space = view_switch_space;
    request = {
      geometry = view_geometry_request;
      state = view_state_request;
    };
  };

  keyboard = {
    key = keyboard_key;
  };

  pointer = {
    button = pointer_button;
    scroll = (fun _ _ _ _ _ _ -> true);
    motion = (fun _ _ _ _ -> true);
  };

  output = {
    created = output_created;
    destroyed = (fun _ _ -> ());
    activated = output_notify;
    resolution = resolution_notify;
  };

  space = {
    created = (fun _ _ -> true);
    destroyed = (fun _ _ -> ());
    activated = active_space;
  }
}

let () =
  Wlc.init interface;
  let _ = Wlc.Compositor.create () in
  Wlc.run ()
