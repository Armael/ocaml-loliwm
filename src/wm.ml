open Batteries

let active = ref None
let cut = ref 0.5

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

  (* TODO: read from the WM's structure describing the views instead of getting
     all the views.  Eg, if we just received a view_destroyed event, the view
     should have been remove from the structure, but still exists (will be
     removed just after). Consequently, View.all_of_space returns it while it
     shouldn't be relayout'd
  *)
  let views = Wlc.View.all_of_space space in
  let count = List.filter is_tiled views |> List.length in
 
 let height = res.h / (if count > 1 then count - 1 else 1) in
  let fheight = if res.h > height * (count - 1) then
      height + (res.h - height * (count - 1))
    else height in

  List.fold_left (fun (toggle, y) v ->
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

let output_created comp output =
  (* add one space *)
  let _ = Wlc.Output.add_space output in
  true

let keyboard_key comp view time modifiers key sym state =
  if modifiers.Wlc.mods = [Config.modifier]
  && state = Wlc.Key_State_Pressed then (
    if sym = Wlc.Keysym._Return then (
      spawn "weston-terminal";
      false
    ) else true
  ) else true

let should_focus_on_create view =
  (* Do not allow unmanaged views to steal focus (tooltips, dnds, etc..) *)
  (* Do not allow parented windows to steal focus, if current window wasn't parent. *)
  let parent = Wlc.View.get_parent view in
  let typ = Wlc.View.get_type view in
  not (List.mem Wlc.View.Unmanaged typ
       && Option.is_some !active
       && Option.is_some parent
       && parent = !active)

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

let view_created comp view space =
  (* TODO: handle & update some internal structure *)

  begin match !active with
  | Some v when is_bemenu view && is_bemenu v ->
    false
  | _ ->
    if should_focus_on_create view then
      set_active comp (Some view);

    relayout space;
    Wlc.log "new view";
    true
  end

let view_destroyed comp view =
  (* TODO: handle & update some internal structure *)

  if !active = Some view then (
    active := None;

    match Wlc.View.get_parent view with
    | Some parent ->
      (* Focus the parent view, if there was one *)
      (* Set parent to None befor this to avoid focusing back to the dying
         view *)
      Wlc.View.set_parent view None;
      set_active comp (Some parent)
    | None ->
      (* Otherwise focus previous one (stacking order) *)
      if not (Wlc.View.is_single view) then
        set_active comp (Some (Wlc.View.get_prev view))
  );

  let space = Wlc.View.get_space view in
  relayout space

let interface = Wlc.Interface.{
  view = {
    created = view_created;
    destroyed = view_destroyed;
    switch_space = (fun _ _ _ _ -> ());
    request = {
      geometry = (fun _ _ _ -> ());
      state = (fun _ _ _ _ -> ());
    };
  };

  keyboard = {
    key = keyboard_key;
  };

  pointer = {
    button = (fun _ _ _ _ _ _ -> true);
    scroll = (fun _ _ _ _ _ _ -> true);
    motion = (fun _ _ _ _ -> true);
  };

  output = {
    created = output_created;
    destroyed = (fun _ _ -> ());
    activated = (fun _ _ -> ());
    resolution = (fun _ _ _ -> ());
  };

  space = {
    created = (fun _ _ -> true);
    destroyed = (fun _ _ -> ());
    activated = (fun _ _ -> ());
  }
}

let () =
  Wlc.init interface;
  let _ = Wlc.Compositor.create () in
  Wlc.run ()
