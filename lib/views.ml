open Tyxml.Html

let pp_ptime ppf ptime =
  let (y, m, d), ((hh, mm, ss), _) = Ptime.to_date_time ptime in
  Fmt.pf ppf "%04d-%02d-%02d %02d:%02d:%02dZ" y m d hh mm ss

let txtf fmt = Fmt.kstr txt fmt
let a_titlef fmt = Fmt.kstr a_title fmt

let check_icon result =
  match result with
  | Builder.Exited 0 ->
    span ~a:[
      a_style "color: green; cursor: pointer;";
      a_titlef "%a" Builder.pp_execution_result result;
    ]
      [txt "☑"]
  | _ ->
    span ~a:[
      a_style "color: red; cursor: pointer;";
      a_titlef "%a" Builder.pp_execution_result result;
    ]
      [txt "☒"]

type nav = [
  | `Default
  | `Job of string * string option
  | `Build of string * Builder_db.Build.t
  | `Comparison of (string * Builder_db.Build.t) * (string * Builder_db.Build.t)
]

let pp_platform = Fmt.(option ~none:(any "") (append (any "on ") string))
let pp_platform_query = Fmt.(option ~none:(any "") (append (any "?platform=") string))

let static_css = Tyxml.Html.Unsafe.data {|
body {
  margin: 40px auto;
  line-height: 1.6;
  color: #444;
  padding: 0 10px;
}
nav ul {
  display: flex;
  list-style: none;
}
nav ul li::before {
  content: "→";
}
nav ul li:first-child::before {
  content: "";
}
nav a {
  padding: .5em 1em;
}
h1,h2,h3{line-height:1.2}
.output-ts {
  white-space: nowrap;
  cursor: pointer;
  user-select: none;
}
.output-ts a {text-decoration: none;}
.output-ts a:hover {text-decoration: underline;}
.output-code {
  overflow: visible;
  white-space: pre;
}
.toggleable {
  display: none;
}
.toggleable-descr {
  cursor: pointer;
  text-decoration: underline;
  user-select: none;
}
:checked + .toggleable {
  display: block;
}
|}

let list_of_option = function
  | Some v -> [ v ]
  | None -> [] 

let layout ?include_static_css ?nav:(nav_=`Default) ~title:title_ body_ =
  let breadcrumb =
    let to_nav kvs =
      nav [ ul (List.map (fun (desc, href) ->
        li [a ~a:[a_href href] [desc]])
        kvs) ]
    in
    match nav_ with
    | `Default ->
      to_nav [txt "Home", "/"]
    | `Job (job_name, platform) ->
      to_nav [txt "Home", "/"; txtf "Job %s" job_name, Fmt.str "/job/%s/" job_name ; txtf "%a" pp_platform platform, Fmt.str "/job/%s/%a" job_name pp_platform_query platform ]
    | `Build (job_name, build) ->
      to_nav [
        txt "Home", "/";
        txtf "Job %s" job_name, Fmt.str "/job/%s/" job_name;
        txtf "%a" pp_platform (Some build.Builder_db.Build.platform), Fmt.str "/job/%s/%a" job_name pp_platform_query (Some build.Builder_db.Build.platform);
        txtf "Build %a" pp_ptime build.Builder_db.Build.start,
        Fmt.str "/job/%s/build/%a/" job_name Uuidm.pp build.Builder_db.Build.uuid;
      ]
    | `Comparison ((job_left, build_left), (job_right, build_right)) ->
      to_nav [
        txt "Home", "/";
        txtf "Comparison between %s@%a and %s@%a"
          job_left pp_ptime build_left.Builder_db.Build.start
          job_right pp_ptime build_right.Builder_db.Build.start,
        Fmt.str "/compare/%a/%a/" Uuidm.pp build_left.uuid Uuidm.pp build_right.uuid;
      ]
  in
  (*> Note: Last declared CSS wins - so one can override here*)
  let static_css = static_css :: list_of_option include_static_css 
  in
  html
    (head (title (txt title_))
       [style ~a:[a_mime_type "text/css"] static_css])

    (body [
        breadcrumb;
        main body_
      ])

let toggleable ?(hidden=true) id description content =
  let checked = if hidden then [] else [a_checked ()] in
  div [
    label
      ~a:[
        a_label_for id;
        a_class ["toggleable-descr"];
      ]
      [txt description];
    input
      ~a:(checked @ [
          a_input_type `Checkbox;
          a_id id;
          a_style "display: none;";
        ]) ();
    div
      ~a:[
        a_class ["toggleable"]
      ]
      content;
  ]

let artifact ?(basename=false) job_name build { Builder_db.filepath; localpath = _; sha256; size } =
  [
    a ~a:[a_href (Fmt.str "/job/%s/build/%a/f/%a"
                    job_name
                    Uuidm.pp build.Builder_db.Build.uuid
                    Fpath.pp filepath)]
      [if basename
       then txt (Fpath.basename filepath)
       else txtf "%a" Fpath.pp filepath];
    txt " ";
    code [txtf "SHA256:%a" Hex.pp (Hex.of_cstruct sha256)];
    txtf " (%a)" Fmt.byte_size size;
  ]



let builder section_job_map =
  layout ~title:"Reproducible OPAM builds"
    ([ h1 [txt "Reproducible OPAM builds"];
       p [ txt "This website offers binary MirageOS unikernels and supplementary OS packages." ];
       p [ txt {|Following is a list of jobs that are built daily. A persistent link to the latest successful build is available as /job/*jobname*/build/latest/. All builds can be reproduced with |} ;
           a ~a:[a_href "https://github.com/roburio/orb/"] [txt "orb"];
           txt ". The builds are scheduled and executed by ";
           a ~a:[a_href "https://github.com/roburio/builder/"] [txt "builder"];
           txt ". The web interface is ";
           a ~a:[a_href "https://git.robur.io/robur/builder-web/"] [txt "builder-web"];
           txt ". Contact team@robur.coop if you have any questions or suggestions.";
       ];
       form ~a:[a_action "/hash"; a_method `Get]
         [
           label [
             txt "Search artifact by SHA256";
             br ();
             input ~a:[
               a_input_type `Search;
               a_id "sha256";
               a_name "sha256";
             ] ();
           ];
           input ~a:[
             a_input_type `Submit;
             a_value "Search";
           ] ();
         ];
       ] @
       Utils.String_map.fold (fun section jobs acc ->
         acc @ [
           h2 [ txt section ];
           ul (List.map (fun (job_name, synopsis, platform_builds) ->
               li ([
                   a ~a:[a_href ("job/" ^ job_name ^ "/")] [txt job_name];
                   br ();
                   txt (Option.value ~default:"" synopsis);
                   br ()
                   ] @
                   List.flatten
                     (List.map (fun (platform, latest_build, latest_artifact) ->
                       [
                         check_icon latest_build.Builder_db.Build.result;
                         txt " ";
                         a ~a:[Fmt.kstr a_href "job/%s/%a" job_name pp_platform_query (Some platform)][txt platform];
                         txt " ";
                         a ~a:[Fmt.kstr a_href "job/%s/build/%a/" job_name Uuidm.pp
                                         latest_build.Builder_db.Build.uuid]
                           [txtf "%a" pp_ptime latest_build.Builder_db.Build.start];
                         txt " ";
                       ] @ (match latest_artifact with
                            | Some main_binary ->
                              artifact ~basename:true job_name latest_build main_binary
                            | None ->
                              [ txtf "Build failure: %a" Builder.pp_execution_result
                                  latest_build.Builder_db.Build.result ]
                       ) @ [ br () ])
                      platform_builds)
                ))
               jobs)
         ])
        section_job_map
        [] @
       [ p [
             txt "View the latest failed builds ";
             a ~a:[a_href "/failed-builds/"]
               [txt "here"];
             txt "."
    ]])

let safe_omd omd =
  let rec safe_block = function
    | Omd.Paragraph (attr, inline) ->
      safe_inline inline
      |> Option.map (fun inline -> Omd.Paragraph (attr, inline))
    | Omd.List (attr, typ, spacing, blocks) ->
      let blocks = List.filter_map (fun b ->
          let b = List.filter_map safe_block b in
          if b = [] then None else Some b)
          blocks
      in
      if blocks = [] then None else
        Some (Omd.List (attr, typ, spacing, blocks))
    | Omd.Blockquote (attr, blocks) ->
      let blocks = List.filter_map safe_block blocks in
      if blocks = [] then None else
        Some (Omd.Blockquote (attr, blocks))
    | Omd.Heading (attr, level, inline) ->
      safe_inline inline
      |> Option.map (fun inline -> Omd.Heading (attr, level, inline))
    | Omd.Html_block _ -> None
    | Omd.Definition_list (attr, def_elts) ->
      let def_elts = List.filter_map safe_def_elts def_elts in
      if def_elts = [] then None else
        Some (Omd.Definition_list (attr, def_elts))
    | Omd.Code_block _
    | Omd.Thematic_break _ as v -> Some v
  and safe_def_elts { term ; defs } =
    let defs = List.filter_map safe_inline defs in
    safe_inline term
    |> Option.map (fun term -> { Omd.term ; defs })
  and safe_inline = function
    | Concat (attr, inline) ->
      Some (Concat (attr, List.filter_map safe_inline inline))
    | Emph (attr, inline) ->
      safe_inline inline
      |> Option.map (fun inline -> Omd.Emph (attr, inline))
    | Strong (attr, inline) ->
      safe_inline inline
      |> Option.map (fun inline -> Omd.Strong (attr, inline))
    | Link (attr, link) ->
      begin match safe_link link with
        | `No_label | `Relative -> safe_inline link.Omd.label
        | `Link l -> Some (Omd.Link (attr, l))
      end
    | Image (attr, link) ->
      begin match safe_link link with
        | `No_label | `Relative -> None
        | `Link l -> Some (Omd.Image (attr, l))
      end
    | Html _ -> None
    | Text _
    | Code _
    | Hard_break _
    | Soft_break _ as v -> Some v
  and safe_link ({ label ; destination ; _ } as l) =
    let absolute_link =
      String.(length destination >= 2 && equal (sub destination 0 2) "//") ||
      String.(length destination >= 7 && equal (sub destination 0 7) "http://") ||
      String.(length destination >= 8 && equal (sub destination 0 8) "https://")
    in
    if absolute_link then
      match safe_inline label with
      | None -> `No_label
      | Some label -> `Link { l with label }
    else
      `Relative
  in
  List.filter_map safe_block omd

let markdown_to_html data =
  let omd = Omd.of_string data in
  let omd = safe_omd omd in
  Omd.to_html omd

let job ~failed name platform readme builds =
  layout ~nav:(`Job (name, platform)) ~title:(Fmt.str "Job %s %a" name pp_platform platform)
    ((h1 [txtf "Job %s %a" name pp_platform platform] ::
      (match readme with
       | None -> []
       | Some data ->
         [
           h2 ~a:[a_id "readme"] [txt "README"];
           a ~a:[a_href "#builds"] [txt "Skip to builds"];
           Unsafe.data (markdown_to_html data)
         ])) @
     [
      h2 ~a:[a_id "builds"] [txt "Builds"];
      a ~a:[a_href "#readme"] [txt "Back to readme"];
      ul (List.map (fun (build, main_binary) ->
          li ([
              check_icon build.Builder_db.Build.result;
              txtf " %s " build.platform;
              a ~a:[Fmt.kstr a_href "/job/%s/build/%a/" name Uuidm.pp build.Builder_db.Build.uuid]
                [
                  txtf "%a" pp_ptime build.Builder_db.Build.start;
                ];
              txt " ";
            ] @ match main_binary with
            | Some main_binary ->
              artifact ~basename:true name build main_binary
            | None ->
              [ txtf "Build failure: %a" Builder.pp_execution_result
                  build.Builder_db.Build.result ]))
          builds);
      if failed then
        p [ txt "Excluding failed builds " ; a ~a:[a_href "../"] [txt "here"] ; txt "." ]
      else
        p [ txt "Including failed builds " ; a ~a:[a_href "failed/"] [txt "here"] ; txt "." ]
    ])

let job_build
    name
    ({ Builder_db.Build.uuid; start; finish; result; platform; _ } as build)
    artifacts
    same_input_same_output different_input_same_output same_input_different_output
    latest next previous
  =
  let delta = Ptime.diff finish start in
  let body =
    h1 [txtf "Job %s" name] ::
    [
      h2 ~a:[a_id "build"] [txtf "Build %a" pp_ptime start];
      p [txtf "Built on platform %s" platform ];
      p [txtf "Build took %a." Ptime.Span.pp delta ];
      p [txtf "Execution result: %a." Builder.pp_execution_result result];
      h3 [txt "Analysis"];
      p [
        let src = Fmt.str "/job/%s/build/%a/treemap" name Uuidm.pp uuid in
        let style = "width: 50em; height: 50.5em" in (*treemap tries to be square*)
        iframe ~a:[ a_src src; a_title "Binary dissection"; a_style style ] [] ];
      h3 [txt "Build info"];
      ul [
        li [ a ~a:[Fmt.kstr a_href "/job/%s/build/%a/console" name Uuidm.pp uuid]
          [txt "Console output"];
        ];
        li [ a ~a:[Fmt.kstr a_href "/job/%s/build/%a/script" name Uuidm.pp uuid]
          [txt "Build script"];
        ]
      ];
      h3 [txt "Build artifacts"];
      dl (List.concat_map
          (fun { Builder_db.filepath; localpath=_; sha256; size } ->
              let (`Hex sha256_hex) = Hex.of_cstruct sha256 in
              [
                dt [a
                    ~a:[Fmt.kstr a_href "f/%a" Fpath.pp filepath]
                    [code [txtf "%a" Fpath.pp filepath]]];
                dd [
                  code [txt "SHA256:"; txt sha256_hex];
                  txtf " (%a)" Fmt.byte_size size;
                ];
              ])
          artifacts);
      h3 [ txtf "Reproduced by %d builds" (List.length (same_input_same_output @ different_input_same_output))] ;
      ul
        ((List.map (fun { Builder_db.Build.start ; uuid ; platform ; _ } ->
            li [
              txtf "on %s, same input, " platform;
              a ~a:[Fmt.kstr a_href "/job/%s/build/%a/" name Uuidm.pp uuid]
                [txtf "%a" pp_ptime start]
            ])
            same_input_same_output) @
         List.map (fun { Builder_db.Build.start ; uuid = other_uuid ; platform ; _ } ->
           li [
             txtf "on %s, different input, " platform;
             a ~a:[Fmt.kstr a_href "/compare/%a/%a/"
                 Uuidm.pp other_uuid Uuidm.pp uuid]
               [txtf "%a" pp_ptime start]
           ])
           different_input_same_output)
    ] @
    (if same_input_different_output = [] then
        []
      else
        [ h3 [txt "Same input, different output (not reproducible!)"];
          ul (
            List.map (fun { Builder_db.Build.start ; uuid = other_uuid ; platform ; _ } ->
              li [
                txtf "on %s, " platform ;
                a ~a:[Fmt.kstr a_href "/compare/%a/%a/" Uuidm.pp other_uuid Uuidm.pp uuid]
                  [txtf "%a" pp_ptime start]
              ])
              same_input_different_output)
        ]) @
    [ h3 [txt "Comparisons with other builds on the same platform"];
      let opt_build (ctx, build) =
        match build with
        | Some b when not (Uuidm.equal uuid b.Builder_db.Build.uuid) ->
          [ li [ txt ctx;
              a ~a:[Fmt.kstr a_href "/compare/%a/%a/"
                  Uuidm.pp b.uuid Uuidm.pp uuid]
                [txtf "%a" pp_ptime b.start]]
          ]
        | _ -> []
      in
      ul
        (List.concat_map opt_build
            [ ("Latest build ", latest) ;
              ("Later build with different output ", next) ;
              ("Earlier build with different output ", previous) ])
    ]
  in
  layout
    ~nav:(`Build (name, build))
    ~title:(Fmt.str "Job %s %a" name pp_ptime start)
    ~include_static_css:(Unsafe.data Modulectomy.Treemap.Doc.css)
    body

let key_values xs =
  List.concat_map (fun (k, v) -> [ txtf "%s %s" k v ; br () ]) xs

let key_value_changes xs =
  List.concat_map (fun (k, v, v') -> [ txtf "%s %s->%s" k v v' ; br () ]) xs

let packages packages =
  OpamPackage.Set.elements packages
  |> List.concat_map (fun p -> [
        txtf "%a" Opamdiff.pp_opampackage p;
        br ();
      ])

let package_diffs diffs =
  List.concat_map (fun pd -> [
        txtf "%a" Opamdiff.pp_version_diff pd;
        br ();
      ])
    diffs

let opam_diffs diffs =
  List.concat_map (fun pd ->
        h4 [ txtf "%a" Opamdiff.pp_opam_diff pd ] ::
        (match pd.Opamdiff.build with None -> [] | Some a ->
          let l, r = Opamdiff.commands_to_strings a in
          [
            h5 [ txt "build instruction (without common prefix) modifications, old:" ] ;
            code (List.concat_map (fun s -> [ txt s ; br () ]) l) ;
            h5 [ txt "new" ] ;
            code (List.concat_map (fun s -> [ txt s ; br () ]) r)
          ]) @
        (match pd.Opamdiff.install with None -> [] | Some a ->
          let l, r = Opamdiff.commands_to_strings a in
          [
            h5 [ txt "install instruction (without common prefix) modifications, old:" ] ;
            code (List.concat_map (fun s -> [ txt s ; br () ]) l) ;
            h5 [ txt "new" ] ;
            code (List.concat_map (fun s -> [ txt s ; br () ]) r)
         ]) @
      (match pd.Opamdiff.url with None -> [] | Some a ->
          let l, r = Opamdiff.opt_url_to_string a in
          [
            h5 [ txt "URL" ] ;
            txtf "old: %s" l;
            br ();
            txtf "new: %s" r
          ]) @
      [ br () ])
    diffs

let compare_builds job_left job_right
    (build_left : Builder_db.Build.t) (build_right : Builder_db.Build.t)
    (added_env, removed_env, changed_env)
    (added_pkgs, removed_pkgs, changed_pkgs)
    (same, opam_diff, version_diff, left, right) =
  layout
    ~nav:(`Comparison ((job_left, build_left), (job_right, build_right)))
    ~title:(Fmt.str "Comparing builds %a and %a"
              Uuidm.pp build_left.uuid Uuidm.pp build_right.uuid)
    ([
      h1 [txt "Comparing builds"];
      h2 [
        txt "Builds ";
        a ~a:[a_href
               (Fmt.str "/job/%s/build/%a/"
                  job_left
                  Uuidm.pp build_left.uuid)]
          [txtf "%s@%a %a" job_left pp_ptime build_left.start pp_platform (Some build_left.platform)];
        txt " and ";
        a ~a:[a_href
               (Fmt.str "/job/%s/build/%a/"
                  job_right
                  Uuidm.pp build_right.uuid)]
          [txtf "%s@%a %a" job_right pp_ptime build_right.start pp_platform (Some build_right.platform)];
      ];
      h3 [ a ~a:[Fmt.kstr a_href "/compare/%a/%a/" Uuidm.pp build_right.uuid Uuidm.pp build_left.uuid]
           [txt "Compare in reverse direction"]] ;
      ul [
        li [
          a ~a:[a_href "#opam-packages-removed"]
            [txtf "%d opam packages removed" (OpamPackage.Set.cardinal left)]
        ];
        li [
          a ~a:[a_href "#opam-packages-installed"]
            [txtf "%d new opam packages installed" (OpamPackage.Set.cardinal right)]
        ];
        li [
          a ~a:[a_href "#opam-packages-version-diff"]
            [txtf "%d opam packages with version changes" (List.length version_diff)]
        ];
        li [
          a ~a:[a_href "#opam-packages-opam-diff"]
            [txtf "%d opam packages with changes in their opam file" (List.length opam_diff)]
        ];
        li [
          a ~a:[a_href "#opam-packages-unchanged"]
            [txtf "%d opam packages unchanged" (OpamPackage.Set.cardinal same)]
        ];
        li [
         a ~a:[a_href "#env-added"]
            [ txtf "%d environment variables added" (List.length added_env)]
        ];
        li [
         a ~a:[a_href "#env-removed"]
            [ txtf "%d environment variables removed" (List.length removed_env)]
        ];
        li [
         a ~a:[a_href "#env-changed"]
            [ txtf "%d environment variables changed" (List.length changed_env)]
        ];
        li [
         a ~a:[a_href "#pkgs-added"]
            [ txtf "%d system packages added" (List.length added_pkgs)]
        ];
        li [
         a ~a:[a_href "#pkgs-removed"]
            [ txtf "%d system packages removed" (List.length removed_pkgs)]
        ];
        li [
         a ~a:[a_href "#pkgs-changed"]
            [ txtf "%d system packages changed" (List.length changed_pkgs)]
        ];
      ];
      h3 ~a:[a_id "opam-packages-removed"]
        [txt "Opam packages removed"];
      code (packages left);
      h3 ~a:[a_id "opam-packages-installed"]
        [txt "New opam packages installed"];
      code (packages right);
      h3 ~a:[a_id "opam-packages-version-diff"]
        [txt "Opam packages with version changes"];
      code (package_diffs version_diff);
      h3 ~a:[a_id "opam-packages-opam-diff"]
        [txt "Opam packages with changes in their opam file"]] @
      opam_diffs opam_diff @ [
      h3 ~a:[a_id "opam-packages-unchanged"]
        [txt "Unchanged opam packages"];
      code (packages same);
      h3 ~a:[a_id "env-added"] [txt "Environment variables added"];
      code (key_values added_env);
      h3 ~a:[a_id "env-removed"] [txt "Environment variables removed"];
      code (key_values removed_env);
      h3 ~a:[a_id "env-changed"] [txt "Environment variables changed"];
      code (key_value_changes changed_env);
      h3 ~a:[a_id "pkgs-added"] [txt "System packages added"];
      code (key_values added_pkgs);
      h3 ~a:[a_id "pkgs-removed"] [txt "System packages removed"];
      code (key_values removed_pkgs);
      h3 ~a:[a_id "pkgs-changed"] [txt "System packages changed"];
      code (key_value_changes changed_pkgs);
    ])

let failed_builds ~start ~count builds =
  let build (job_name, build) =
    li [
      check_icon build.Builder_db.Build.result;
      txtf " %s %a " job_name pp_platform (Some build.platform);
      a ~a:[Fmt.kstr a_href "/job/%s/build/%a/" job_name Uuidm.pp build.uuid]
        [txtf "%a" pp_ptime build.start];
      txtf " %a" Builder.pp_execution_result build.result;
    ]
  in
  layout ~title:"Failed builds"
    ([ h1 [txt "Failed builds"];
       ul (List.map build builds);
       p [ txtf "View the next %d failed builds " count;
           a ~a:[Fmt.kstr a_href "/failed-builds/?count=%d&start=%d" count (start + count)]
             [ txt "here"];
           txt ".";
       ]
    ])
       
