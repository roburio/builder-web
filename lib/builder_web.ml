let src = Logs.Src.create "builder-web" ~doc:"Builder_web"
module Log = (val Logs.src_log src : Logs.LOG)

open Lwt.Syntax
open Lwt_result.Infix

let pp_error ppf = function
  | #Caqti_error.connect as e -> Caqti_error.pp ppf e
  | #Model.error as e -> Model.pp_error ppf e
  | `Wrong_version (application_id, version) ->
    if application_id = Builder_db.application_id
    then Format.fprintf ppf "Wrong database version: %Ld, expected %Ld" version Builder_db.current_version
    else Format.fprintf ppf "Wrong database application id: %ld, expected %ld" application_id Builder_db.application_id

let init_datadir datadir =
  let ( let* ) = Result.bind and ( let+ ) x f = Result.map f x in
  let* exists = Bos.OS.Dir.exists datadir in
  let* () =
    if exists
    then Ok ()
    else Error (`Msg "Datadir does not exist")
  in
  let+ _ = Bos.OS.Dir.create ~path:false (Model.staging datadir) in
  ()

let init dbpath datadir =
  Result.bind (init_datadir datadir) @@ fun () ->
  Lwt_main.run (
    Caqti_lwt.connect
      (Uri.make ~scheme:"sqlite3" ~path:dbpath ~query:["create", ["false"]] ())
    >>= fun (module Db : Caqti_lwt.CONNECTION) ->
    Db.find Builder_db.get_application_id () >>= fun application_id ->
    Db.find Builder_db.get_version () >>= (fun version ->
        if (application_id, version) = Builder_db.(application_id, current_version)
        then Lwt_result.return ()
        else Lwt_result.fail (`Wrong_version (application_id, version)))
    >>= fun () ->
    Model.cleanup_staging datadir (module Db))

let pp_exec ppf ((job : Builder.script_job), uuid, _, _, _, _, _) =
  Format.fprintf ppf "%s(%a)" job.Builder.name Uuidm.pp uuid

let safe_seg path =
  if Fpath.is_seg path && not (Fpath.is_rel_seg path)
  then Ok (Fpath.v path)
  else Fmt.kstr (fun s -> Error (`Msg s)) "unsafe path %S" path

(* mime lookup with orb knowledge *)
let mime_lookup path =
  match Fpath.to_string path with
  | "build-environment" | "opam-switch" | "system-packages" ->
    "text/plain"
  | filename ->
    if Fpath.has_ext "build-hashes" path
    then "text/plain"
    else if Fpath.is_prefix Fpath.(v "bin/") path
    then "application/octet-stream"
    else match Option.bind
                 (Result.to_option (Conan_unix.run_with_tree Conan_magic_database.tree filename))
                 Conan.Metadata.mime with
      | Some mime_type -> mime_type
      | None -> "application/octet-stream" (* default *)

let string_of_html =
  Format.asprintf "%a" (Tyxml.Html.pp ())

let or_error_response r =
  let* r = r in
  match r with
  | Ok response -> Lwt.return response
  | Error (text, `Not_Found) ->
    Views.resource_not_found ~text
    |> string_of_html |> Dream.html ~status:`Not_Found
  | Error (text, status) -> Dream.respond ~status text

let default_log_warn ~status e =
  Log.warn (fun m -> m "%s: %a" (Dream.status_to_string status) pp_error e)

let if_error
    ?(status = `Internal_Server_Error)
    ?(log = default_log_warn ~status)
    message r =
  let* r = r in
  match r with
  | Error `Not_found ->
    Lwt_result.fail ("Resource not found", `Not_Found)
  | Error (#Model.error as e) ->
    log e;
    Lwt_result.fail (message, status)
  | Ok _ as r -> Lwt.return r

let get_uuid s =
  Lwt.return
    (if String.length s = 36 then
       match Uuidm.of_string s with
       | Some uuid -> Ok uuid
       | None -> Error ("Bad uuid", `Bad_Request)
     else Error ("Bad uuid", `Bad_Request))


let main_binary_of_uuid uuid db =
  Model.build uuid db
  |> if_error "Error getting job build"
    ~log:(fun e -> Log.warn (fun m -> m "Error getting job build: %a" pp_error e))
  >>= fun (_id, build) ->
  match build.Builder_db.Build.main_binary with
  | None -> Lwt_result.fail ("Resource not found", `Not_Found)
  | Some main_binary ->
    Model.build_artifact_by_id main_binary db
    |> if_error "Error getting main binary" 

module Viz_aux = struct

  let viz_type_to_string = function
    | `Treemap -> "treemap"
    | `Dependencies -> "dependencies"

  let viz_dir ~cachedir ~viz_typ ~version =
    let typ_str = viz_type_to_string viz_typ in
    Fpath.(cachedir / Fmt.str "%s_%d" typ_str version)

  let viz_path ~cachedir ~viz_typ ~version ~input_hash =
    Fpath.(
      viz_dir ~cachedir ~viz_typ ~version
      / input_hash + "html"
    )
  
  let choose_versioned_viz_path
      ~cachedir
      ~viz_typ
      ~viz_input_hash
      ~current_version =
    let ( >>= ) = Result.bind in
    let rec aux current_version = 
      let path =
        viz_path ~cachedir
          ~viz_typ
          ~version:current_version
          ~input_hash:viz_input_hash in
      Bos.OS.File.exists path >>= fun path_exists ->
      if path_exists then Ok path else (
        if current_version = 1 then
          Error (`Msg (Fmt.str "viz '%s': There exist no version of the requested \
                                visualization"
                         (viz_type_to_string viz_typ)))
        else 
          aux @@ pred current_version
      )
    in
    aux current_version

  let get_viz_version_from_dirs ~cachedir ~viz_typ =
    let ( >>= ) = Result.bind in
    Bos.OS.Dir.contents cachedir >>= fun versioned_dirs ->
    let max_cached_version = 
      let viz_typ_str = viz_type_to_string viz_typ ^ "_" in
      versioned_dirs
      |> List.filter_map (fun versioned_dir ->
          match Bos.OS.Dir.exists versioned_dir with
          | Error (`Msg err) ->
            Logs.warn (fun m -> m "%s" err);
            None
          | Ok false -> None
          | Ok true -> 
            let dir_str = Fpath.filename versioned_dir in
            if not (String.starts_with ~prefix:viz_typ_str dir_str) then
              None
            else
              try
                String.(sub dir_str
                          (length viz_typ_str)
                          (length dir_str - length viz_typ_str))
                |> int_of_string
                |> Option.some
              with Failure _ ->
                Logs.warn (fun m ->
                    m "Failed to read visualization-version from directory: '%s'"
                      (Fpath.to_string versioned_dir));
                None
        )
      |> List.fold_left Int.max (-1)
    in
    if max_cached_version = -1 then
      Result.error @@
      `Msg (Fmt.str "Couldn't find any visualization-version of %s"
              (viz_type_to_string viz_typ))
    else
      Result.ok max_cached_version

  let hash_viz_input ~uuid typ db =
    let open Builder_db in
    let hex cstruct =
      let `Hex hex_str = Hex.of_cstruct cstruct in
      hex_str
    in
    main_binary_of_uuid uuid db >>= fun main_binary ->
    Model.build uuid db
    |> if_error "Error getting build" >>= fun (build_id, _build) ->
    Model.build_artifacts build_id db
    |> if_error "Error getting build artifacts" >>= fun artifacts ->
    match typ with
    | `Treemap ->
      let debug_binary = 
        let bin = Fpath.base main_binary.localpath in
        List.find_opt
          (fun p -> Fpath.(equal (bin + "debug") (base p.localpath)))
          artifacts
      in
      begin
        match debug_binary with
        | None -> Lwt_result.fail ("Error getting debug-binary", `Not_Found)
        | Some debug_binary ->
          debug_binary.sha256
          |> hex
          |> Lwt_result.return
      end 
    | `Dependencies -> 
      let opam_switch =
        List.find_opt
          (fun p -> Fpath.(equal (v "opam-switch") (base p.localpath)))
          artifacts
      in
      match opam_switch with
      | None -> Lwt_result.fail ("Error getting opam-switch", `Not_Found)
      | Some opam_switch ->
        opam_switch.sha256
        |> hex
        |> Lwt_result.return

  let try_load_cached_visualization ~cachedir ~uuid viz_typ db =
    Lwt.return (get_viz_version_from_dirs ~cachedir ~viz_typ)
    |> if_error "Error getting visualization version" >>= fun latest_viz_version ->
    hash_viz_input ~uuid viz_typ db >>= fun viz_input_hash ->
    (choose_versioned_viz_path
       ~cachedir
       ~current_version:latest_viz_version
       ~viz_typ
       ~viz_input_hash
     |> Lwt.return
     |> if_error "Error finding a version of the requested visualization")
    >>= fun viz_path ->
    Lwt_result.catch (
      Lwt_io.with_file ~mode:Lwt_io.Input
        (Fpath.to_string viz_path)
        Lwt_io.read
    )
    |> Lwt_result.map_err (fun exn -> `Msg (Printexc.to_string exn))
    |> if_error "Error getting cached visualization"

end


let routes ~datadir ~cachedir ~configdir =
  let builds req =
    Dream.sql req Model.jobs_with_section_synopsis
    |> if_error "Error getting jobs"
      ~log:(fun e -> Log.warn (fun m -> m "Error getting jobs: %a" pp_error e))
    >>= fun jobs ->
    List.fold_right
      (fun (job_id, job_name, section, synopsis) r ->
         r >>= fun acc ->
         Dream.sql req (Model.platforms_of_job job_id) >>= fun ps ->
         List.fold_right (fun platform r ->
           r >>= fun acc ->
           Dream.sql req (Model.build_with_main_binary job_id platform) >>= function
           | Some (build, artifact) ->
             Lwt_result.return ((platform, build, artifact) :: acc)
           | None ->
             Log.warn (fun m -> m "Job without builds: %s" job_name);
             Lwt_result.return acc)
           ps (Lwt_result.return []) >>= fun platform_builds ->
         let v = (job_name, synopsis, platform_builds) in
         let section = Option.value ~default:"Uncategorized" section in
         Lwt_result.return (Utils.String_map.add_or_create section v acc))
      jobs
      (Lwt_result.return Utils.String_map.empty)
    |> if_error "Error getting jobs"
      ~log:(fun e -> Log.warn (fun m -> m "Error getting jobs: %a" pp_error e))
    >>= fun jobs ->
    Views.Builds.make jobs |> string_of_html |> Dream.html |> Lwt_result.ok
  in

  let job req =
    let job_name = Dream.param req "job" in
    let platform = Dream.query req "platform" in
    (Dream.sql req (Model.job_and_readme job_name) >>= fun (job_id, readme) ->
     Dream.sql req (Model.builds_grouped_by_output job_id platform) >|= fun builds ->
     (readme, builds))
    |> if_error "Error getting job"
      ~log:(fun e -> Log.warn (fun m -> m "Error getting job: %a" pp_error e))
    >>= fun (readme, builds) ->
    Views.Job.make ~failed:false ~job_name ~platform ~readme builds
    |> string_of_html |> Dream.html |> Lwt_result.ok
  in

  let job_with_failed req =
    let job_name = Dream.param req "job" in
    let platform = Dream.query req "platform" in
    (Dream.sql req (Model.job_and_readme job_name) >>= fun (job_id, readme) ->
     Dream.sql req (Model.builds_grouped_by_output_with_failed job_id platform) >|= fun builds ->
     (readme, builds))
    |> if_error "Error getting job"
      ~log:(fun e -> Log.warn (fun m -> m "Error getting job: %a" pp_error e))
    >>= fun (readme, builds) ->
    Views.Job.make ~failed:true ~job_name ~platform ~readme builds
    |> string_of_html |> Dream.html |> Lwt_result.ok
  in

  let redirect_latest req =
    let job_name = Dream.param req "job" in
    let platform = Dream.query req "platform" in
    (* FIXME *)
    let path = begin[@alert "-deprecated"] Dream.path req |> String.concat "/" end in
    (Dream.sql req (Model.job_id job_name) >>= Model.not_found >>= fun job_id ->
     Dream.sql req (Model.latest_successful_build_uuid job_id platform))
    >>= Model.not_found
    |> if_error "Error getting job" >>= fun build ->
    Dream.redirect req
      (Fmt.str "/job/%s/build/%a/%s" job_name Uuidm.pp build path)
    |> Lwt_result.ok
  in

  let redirect_main_binary req =
    let job_name = Dream.param req "job"
    and build = Dream.param req "build" in
    get_uuid build >>= fun uuid ->
    Dream.sql req (main_binary_of_uuid uuid) >>= fun main_binary ->
    Dream.redirect req
      (Fmt.str "/job/%s/build/%a/f/%a" job_name Uuidm.pp uuid
         Fpath.pp main_binary.Builder_db.filepath)
    |> Lwt_result.ok
  in

  let job_build_viz viz_typ req =
    let _job_name = Dream.param req "job"
    and build = Dream.param req "build" in
    get_uuid build >>= fun uuid ->
    Dream.sql req (Viz_aux.try_load_cached_visualization ~cachedir ~uuid viz_typ)
    >>= fun svg_html ->
    Lwt_result.ok (Dream.html svg_html)
  in

  let job_build req =
    let job_name = Dream.param req "job"
    and build = Dream.param req "build" in
    get_uuid build >>= fun uuid ->
    Dream.sql req (fun conn ->
        Model.build uuid conn >>= fun (build_id, build) ->
        (match build.Builder_db.Build.main_binary with
         | Some main_binary ->
           Model.build_artifact_by_id main_binary conn |> Lwt_result.map Option.some
         | None -> Lwt_result.return None) >>= fun main_binary ->
        Model.build_artifacts build_id conn >>= fun artifacts ->
        Model.builds_with_same_input_and_same_main_binary build_id conn >>= fun same_input_same_output ->
        Model.builds_with_different_input_and_same_main_binary build_id conn >>= fun different_input_same_output ->
        Model.builds_with_same_input_and_different_main_binary build_id conn >>= fun same_input_different_output ->
        Model.latest_successful_build build.job_id (Some build.Builder_db.Build.platform) conn >>= fun latest ->
        Model.next_successful_build_different_output build_id conn >>= fun next ->
        Model.previous_successful_build_different_output build_id conn >|= fun previous ->
        (build, main_binary, artifacts, same_input_same_output, different_input_same_output, same_input_different_output, latest, next, previous)
      )
    |> if_error "Error getting job build"
      ~log:(fun e -> Log.warn (fun m -> m "Error getting job build: %a" pp_error e))
    >>= fun (build, main_binary, artifacts, same_input_same_output, different_input_same_output, same_input_different_output, latest, next, previous) ->
    let solo5_manifest = Option.bind main_binary (Model.solo5_manifest datadir) in
    Views.Job_build.make
      ~name:job_name
      ~build
      ~artifacts
      ~main_binary
      ~solo5_manifest
      ~same_input_same_output
      ~different_input_same_output
      ~same_input_different_output
      ~latest ~next ~previous
    |> string_of_html |> Dream.html |> Lwt_result.ok
  in

  let job_build_file req =
    let _job_name = Dream.param req "job"
    and build = Dream.param req "build"
    (* FIXME *)
    and filepath = begin[@alert "-deprecated"] Dream.path req |> String.concat "/" end in
    let if_none_match = Dream.header req "if-none-match" in
    (* XXX: We don't check safety of [file]. This should be fine however since
     * we don't use [file] for the filesystem but is instead used as a key for
     * lookup in the data table of the 'full' file. *)
    get_uuid build >>= fun build ->
    Fpath.of_string filepath |> Lwt_result.lift
    |> if_error ~status:`Not_Found "File not found" >>= fun filepath ->
    Dream.sql req (Model.build_artifact build filepath)
    |> if_error "Error getting build artifact" >>= fun file ->
    let etag = Base64.encode_string (Cstruct.to_string file.Builder_db.sha256) in
    match if_none_match with
    | Some etag' when etag = etag' ->
      Dream.empty `Not_Modified |> Lwt_result.ok
    | _ ->
      Model.build_artifact_data datadir file
      |> if_error "Error getting build artifact"
          ~log:(fun e -> Log.warn (fun m -> m "Error getting build artifact data for file %a in %a: %a"
                           Fpath.pp file.Builder_db.filepath Fpath.pp file.Builder_db.localpath
                           pp_error e)) >>= fun data ->
      let headers = [
        "Content-Type", mime_lookup file.Builder_db.filepath;
        "ETag", etag;
      ] in
      Dream.respond ~headers data |> Lwt_result.ok
  in

  let job_build_static_file (file : [< `Console | `Script ]) req =
    let _job_name = Dream.param req "job"
    and build = Dream.param req "build" in
    get_uuid build >>= fun build ->
    (match file with
    | `Console ->
      Dream.sql req (Model.build_console_by_uuid datadir build)
    | `Script ->
      Dream.sql req (Model.build_script_by_uuid datadir build))
    |> if_error "Error getting data"
      ~log:(fun e -> Log.warn (fun m -> m "Error getting script or console data for build %a: %a"
                                  Uuidm.pp build pp_error e)) >>= fun data ->
    let headers = [ "Content-Type", "text/plain" ] in
    Dream.respond ~headers data |> Lwt_result.ok
  in

  let failed_builds req =
    let platform = Dream.query req "platform" in
    let to_int default s = Option.(value ~default (bind s int_of_string_opt)) in
    let start = to_int 0 (Dream.query req "start") in
    let count = to_int 10 (Dream.query req "count") in
    Dream.sql req (Model.failed_builds ~start ~count platform)
    |> if_error "Error getting data"
       ~log:(fun e -> Log.warn (fun m -> m "Error getting failed builds: %a"
                                  pp_error e)) >>= fun builds ->
    Views.failed_builds ~start ~count builds
    |> string_of_html |> Dream.html |> Lwt_result.ok
  in

  let job_build_targz req =
    let _job_name = Dream.param req "job"
    and build = Dream.param req "build" in
    get_uuid build >>= fun build ->
    Dream.sql req (Model.build build)
    |> if_error "Error getting build" >>= fun (build_id, build) ->
    Dream.sql req (Model.build_artifacts build_id)
    |> if_error "Error getting artifacts" >>= fun artifacts ->
    Ptime.diff build.finish Ptime.epoch |> Ptime.Span.to_int_s
    |> Option.to_result ~none:(`Msg "bad finish time") |> Result.map Int64.of_int
    |> Lwt.return |> if_error "Internal server error" >>= fun finish ->
    Dream.stream ~headers:["Content-Type", "application/tar+gzip"]
      (Dream_tar.targz_response datadir finish artifacts)
    |> Lwt_result.ok
  in

  let upload req =
    let* body = Dream.body req in
    Builder.Asn.exec_of_cs (Cstruct.of_string body) |> Lwt.return
    |> if_error ~status:`Bad_Request "Bad request"
      ~log:(fun e ->
        Log.warn (fun m -> m "Received bad builder ASN.1: %a" pp_error e))
    >>= fun ((({ name ; _ } : Builder.script_job), uuid, _, _, _, _, _) as exec) ->
    Log.debug (fun m -> m "Received build %a" pp_exec exec);
    Authorization.authorized req name
    |> if_error ~status:`Forbidden "Forbidden" >>= fun () ->
    Dream.sql req (Model.build_exists uuid)
    |> if_error "Internal server error"
      ~log:(fun e ->
              Log.warn (fun m -> m "Error saving build %a: %a" pp_exec exec pp_error e))
    >>= function
    | true ->
      Log.warn (fun m -> m "Build with same uuid exists: %a" pp_exec exec);
      Dream.respond ~status:`Conflict
        (Fmt.str "Build with same uuid exists: %a\n" Uuidm.pp uuid)
      |> Lwt_result.ok
    | false ->
      (Lwt.return (Dream.field req Authorization.user_info_field |>
                   Option.to_result ~none:(`Msg "no authenticated user")) >>= fun (user_id, _) ->
       Dream.sql req (Model.add_build ~datadir ~cachedir ~configdir user_id exec))
      |> if_error "Internal server error"
        ~log:(fun e -> Log.warn (fun m -> m "Error saving build %a: %a" pp_exec exec pp_error e))
      >>= fun () -> Dream.respond "" |> Lwt_result.ok
  in

  let hash req =
    Dream.query req "sha256" |> Option.to_result ~none:(`Msg "Missing sha256 query parameter")
    |> Lwt.return
    |> if_error ~status:`Bad_Request "Bad request" >>= fun hash_hex ->
    begin try Hex.to_cstruct (`Hex hash_hex) |> Lwt_result.return
      with Invalid_argument e -> Lwt_result.fail (`Msg ("Bad hex: " ^ e))
    end
    |> if_error ~status:`Bad_Request "Bad request" >>= fun hash ->
    Dream.sql req (Model.build_hash hash) >>= Model.not_found
    |> if_error "Internal server error" >>= fun (job_name, build) ->
    Dream.redirect req
      (Fmt.str "/job/%s/build/%a" job_name Uuidm.pp build.Builder_db.Build.uuid)
    |> Lwt_result.ok
  in

  let compare_builds req =
    let build_left = Dream.param req "build_left" in
    let build_right = Dream.param req "build_right" in
    get_uuid build_left >>= fun build_left ->
    get_uuid build_right >>= fun build_right ->
    Dream.sql req (fun conn ->
        Model.build build_left conn >>= fun (_id, build_left) ->
        Model.build build_right conn >>= fun (_id, build_right) ->
        Model.build_artifact build_left.Builder_db.Build.uuid (Fpath.v "opam-switch") conn >>=
        Model.build_artifact_data datadir >>= fun switch_left ->
        Model.build_artifact build_left.Builder_db.Build.uuid (Fpath.v "build-environment") conn >>=
        Model.build_artifact_data datadir >>= fun build_env_left ->
        Model.build_artifact build_left.Builder_db.Build.uuid (Fpath.v "system-packages") conn >>=
        Model.build_artifact_data datadir >>= fun system_packages_left ->
        Model.build_artifact build_right.Builder_db.Build.uuid (Fpath.v "opam-switch") conn >>=
        Model.build_artifact_data datadir >>= fun switch_right ->
        Model.build_artifact build_right.Builder_db.Build.uuid (Fpath.v "build-environment") conn >>=
        Model.build_artifact_data datadir >>= fun build_env_right ->
        Model.build_artifact build_right.Builder_db.Build.uuid (Fpath.v "system-packages") conn >>=
        Model.build_artifact_data datadir >>= fun system_packages_right ->
        Model.job_name build_left.job_id conn >>= fun job_left ->
        Model.job_name build_right.job_id conn >|= fun job_right ->
        (job_left, job_right, build_left, build_right,
         switch_left, build_env_left, system_packages_left,
         switch_right, build_env_right, system_packages_right))
    |> if_error "Internal server error"
    >>= fun (job_left, job_right, build_left, build_right,
             switch_left, build_env_left, system_packages_left,
             switch_right, build_env_right, system_packages_right) ->
    let env_diff = Utils.compare_env build_env_left build_env_right
    and pkg_diff = Utils.compare_pkgs system_packages_left system_packages_right
    in
    let switch_left = OpamFile.SwitchExport.read_from_string switch_left
    and switch_right = OpamFile.SwitchExport.read_from_string switch_right in
    let opam_diff = Opamdiff.compare switch_left switch_right in
    Views.compare_builds
      ~job_left ~job_right
      ~build_left ~build_right
      ~env_diff
      ~pkg_diff
      ~opam_diff
    |> string_of_html |> Dream.html |> Lwt_result.ok
  in

  let upload_binary req =
    let job = Dream.param req "job" in
    let platform = Dream.param req "platform" in
    let binary_name =
      Dream.query req "binary_name"
      |> Option.map Fpath.of_string
      |> Option.value ~default:(Ok Fpath.(v job + "bin"))
    in
    if_error "Bad request" ~status:`Bad_Request (Lwt.return binary_name) >>=
    fun binary_name ->
    let* body = Dream.body req in
    Authorization.authorized req job
    |> if_error ~status:`Forbidden "Forbidden" >>= fun () ->
    let uuid = Uuidm.v4_gen (Random.State.make_self_init ()) () in
    Dream.sql req (Model.build_exists uuid)
    |> if_error "Internal server error"
      ~log:(fun e ->
              Log.warn (fun m -> m "Error saving binary %S: %a" job pp_error e))
    >>= function
    | true ->
      Log.warn (fun m -> m "Build %S with same uuid exists: %a" job Uuidm.pp uuid);
      Dream.respond ~status:`Conflict
        (Fmt.str "Build with same uuid exists: %a\n" Uuidm.pp uuid)
      |> Lwt_result.ok
    | false ->
      let exec =
        let now = Ptime_clock.now () in
        ({ Builder.name = job ; platform ; script = "" }, uuid, [], now, now, Builder.Exited 0,
         [ (Fpath.(v "bin" // binary_name), body) ])
      in
      (Lwt.return (Dream.field req Authorization.user_info_field |>
                   Option.to_result ~none:(`Msg "no authenticated user")) >>= fun (user_id, _) ->
       Dream.sql req (Model.add_build ~datadir ~cachedir ~configdir user_id exec))
      |> if_error "Internal server error"
        ~log:(fun e -> Log.warn (fun m -> m "Error saving build %a: %a" pp_exec exec pp_error e))
      >>= fun () -> Dream.respond "" |> Lwt_result.ok
  in

  let redirect_parent req =
    let queries = Dream.all_queries req in
    let parent_url =
      let parent_path =
        Dream.target req
        |> Utils.Path.of_url
        |> List.rev |> List.tl |> List.rev
      in
      Utils.Path.to_url ~path:parent_path ~queries
    in
    Dream.redirect ~status:`Temporary_Redirect req parent_url
    |> Lwt_result.ok
  in

  let w f req = or_error_response (f req) in

  [
    Dream.get "/" (w builds);
    Dream.get "/job" (w redirect_parent);
    Dream.get "/job/:job" (w job);
    Dream.get "/job/:job/build" (w redirect_parent);
    Dream.get "/job/:job/failed" (w job_with_failed);
    Dream.get "/job/:job/build/latest/**" (w redirect_latest);
    Dream.get "/job/:job/build/:build" (w job_build);
    Dream.get "/job/:job/build/:build/f/**" (w job_build_file);
    Dream.get "/job/:job/build/:build/main-binary" (w redirect_main_binary);
    Dream.get "/job/:job/build/:build/viztreemap" (w @@ job_build_viz `Treemap);
    Dream.get "/job/:job/build/:build/vizdependencies" (w @@ job_build_viz `Dependencies);
    Dream.get "/job/:job/build/:build/script" (w (job_build_static_file `Script));
    Dream.get "/job/:job/build/:build/console" (w (job_build_static_file `Console));
    Dream.get "/failed-builds" (w failed_builds);
    Dream.get "/job/:job/build/:build/all.tar.gz" (w job_build_targz);
    Dream.get "/hash" (w hash);
    Dream.get "/compare/:build_left/:build_right" (w compare_builds);
    Dream.post "/upload" (Authorization.authenticate (w upload));
    Dream.post "/job/:job/platform/:platform/upload" (Authorization.authenticate (w upload_binary));
  ]

let routeprefix_ignorelist_when_removing_trailing_slash = [
  "/job/:job/build/:build/f"
]

module Middleware = struct

  let remove_trailing_url_slash : Dream.middleware =
    fun handler req ->
      let path = Dream.target req |> Utils.Path.of_url in
      let is_ignored =
        routeprefix_ignorelist_when_removing_trailing_slash
        |> List.exists (Utils.Path.matches_dreamroute ~path)
      in
      if not (List.mem (Dream.method_ req) [`GET; `HEAD]) || is_ignored then
        handler req
      else match List.rev path with
        | "" :: [] (* / *) -> handler req
        | "" :: path (* /.../ *) ->
          let path = List.rev path in
          let queries = Dream.all_queries req in
          let url = Utils.Path.to_url ~path ~queries in
          (*> Note: See https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Location*)
          Dream.redirect ~status:`Permanent_Redirect req url
        | _ (* /... *) -> handler req

end
