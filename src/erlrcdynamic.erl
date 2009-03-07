%% @doc Dynamic hooks intended to be called from packaging systems.
%% Includes routines for starting, stopping, upgrading, and downgrading
%% applications.  Automatically generates requisite .appup files (only if
%% they don't exist already).
%% @end

-module (erlrcdynamic).
-export ([ downgrade/3,
           downgrade/5,
           local_error_msg/2,
           start/2,
           start/3,
           stop/2,
           unload/2,
           unload/3,
           upgrade/3,
           upgrade/5 ]).

-include_lib ("kernel/include/file.hrl").

-ifdef (HAVE_EUNIT).
-include_lib ("eunit/include/eunit.hrl").
-endif.

%-=====================================================================-
%-                                Public                               -
%-=====================================================================-

%% @spec downgrade (atom(), string(), string())
%%         -> { ok, Reason::atom () }
%%          | { ok, [ Unpurged ] }
%%          | restart_new_emulator
%%          | { error, Reason }
%% @equiv downgrade (Application,
%%                   EarlierVersion,
%%                   LaterVersion,
%%                   EarlierDir,
%%                   LaterDir)
%% @doc Downgrade Application from LaterVersion to EarlierVersion.
%% Generates an .appup file if one does not exist already, and then
%% calls release_handler:downgrade_app/2. The two versions' directories
%% EarlierDir and LaterDir are assumed to be in the standard location
%% under code:lib_dir().
%% @end

downgrade (Application, EarlierVersion, LaterVersion) ->
  Prefix = code:lib_dir () ++ "/" ++ atom_to_list (Application) ++ "-",
  EarlierDir = Prefix ++ EarlierVersion,
  LaterDir = Prefix ++ LaterVersion,
  downgrade (Application, EarlierVersion, LaterVersion, EarlierDir, LaterDir).

%% @spec downgrade (atom(), string(), string(), string(), string())
%%         -> { ok, Reason::atom () }
%%          | { ok, [ Unpurged ] }
%%          | restart_new_emulator
%%          | { error, Reason }
%% @doc Downgrade Application from LaterVersion in LaterDir to
%% EarlierVersion in EarlierDir.  Generates an .appup file if one does
%% not exist already, and then calls release_handler:downgrade_app/2.
%% @end

downgrade (Application, EarlierVersion, LaterVersion, EarlierDir, LaterDir) ->
  protect_state (fun () ->
		   downgrade_unprotected (Application,
					  EarlierVersion,
					  LaterVersion,
					  EarlierDir,
					  LaterDir)
		 end).

downgrade_unprotected (Application,
		       EarlierVersion,
		       LaterVersion,
		       EarlierDir,
		       LaterDir) ->
  case lists:keysearch (Application, 1, application:which_applications ()) of
    { value, { Application, _, EarlierVersion } } ->
      { ok, already_running };
    { value, { Application, _, LaterVersion } } ->
      do_downgrade (Application,
		    EarlierVersion,
		    LaterVersion,
		    EarlierDir,
		    LaterDir);
    false ->
      case file:read_file_info (LaterDir) of
        { ok, _ } ->
          case do_downgrade (Application,
                             EarlierVersion,
                             LaterVersion,
                             EarlierDir,
                             LaterDir)
	  of
            R = { ok, _ } ->
	      case start (Application, EarlierVersion, EarlierDir) of
		included -> R;
		already_running -> R;
		started -> R;
		started_included_stopped -> R;
		version_mismatch -> { error, version_mismatch };
		version_load_mismatch -> { error, version_load_mismatch }
	      end;
            X ->
              X
          end;
        { error, enoent } ->
           % ok we'll interpret this to mean the new version was never
           % installed.  TODO: is there something better?
           case start (Application, EarlierVersion, EarlierDir) of
             included -> { ok, included };
             already_running -> { ok, already_running };
             started -> { ok, started };
             started_included_stopped -> { ok, started_included_stopped };
             version_mismatch -> { error, version_mismatch };
             version_load_mismatch -> { error, version_load_mismatch }
           end
      end;
    { value, { Application, _, OtherVersion } } ->
      local_error_msg ("erlrcdynamic:upgrade/5: got OtherVersion '~p' "
                       "which is neither LaterVersion '~p' nor "
		       "EarlierVersion '~p'~n",
                       [ OtherVersion, LaterVersion, EarlierVersion ]),
      { error, version_mismatch }
  end.

%% @hidden

local_error_msg (Format, Args) ->
  Leader = erlang:group_leader (),
  try
    true = erlang:group_leader (self (), self ()),
    error_logger:error_msg (Format, Args)
  after
    erlang:group_leader (Leader, self ())
  end.

%% @spec start (atom(), string())
%%         -> already_running
%%          | started
%%          | version_mismatch
%%          | version_load_mismatch
%%          | bad_directory
%% @equiv start (Application, Version, Dir)
%% @doc Start the given Version of the given Application. Assumes the
%% application's directory is in the standard location under code:lib_dir().
%% @end

start (Application, Version) ->
  Dir = code:lib_dir (),
  AppDir = Dir ++ "/" ++ atom_to_list (Application) ++ "-" ++ Version,
  start (Application, Version, AppDir).

%% @spec start (atom(), string(), string())
%%         -> already_running
%%          | started
%%          | started_included_stopped
%%          | included
%%          | version_mismatch
%%          | version_load_mismatch
%%          | bad_directory
%% @doc Start the given Version of Application located in AppDir.
%% Returns:
%% <ul>
%% <li>'already_running' if that version of the application was already
%%   started;</li>
%% <li>'started' if the application is succesfully started;</li>
%% <li>'started_included_stopped' if the application is successfully
%%   started and any included applications were successfully stopped;</li>
%% <li>'included' if application is included in another application
%%   (and does not start it);</li>
%% <li>'version_mismatch' if the application is running with a different
%%   version (use {@link upgrade/5} instead);</li>
%% <li>'version_load_mismatch' if the application was attempted to be
%%   loaded, but a different version was found;</li>
%% <li>'bad_directory' if AppDir/ebin does not exist.</li>
%% </ul>
%% @end

start (Application, Version, AppDir) ->
  protect_state (fun () ->
		   start_unprotected (Application, Version, AppDir)
		 end,
		 fun (R) ->
		   % this is the only error after messing with code path
		   R =:= version_load_mismatch
		 end).

start_unprotected (App, Version, AppDir) ->
  case lists:keysearch (App, 1, application:which_applications ()) of
    { value, { App, _, Version } } ->
      already_running;
    { value, { App, _, OtherVersion }} ->
      local_error_msg ("erlrcdynamic:start/3: got OtherVersion '~p' "
                       "which is not Version '~p'~n",
                       [ OtherVersion, Version ]),
      version_mismatch;
    false ->
      EbinDir = AppDir ++ "/ebin",
      case code:add_patha (EbinDir) of
        true ->
          case lists:keysearch (App, 1, application:loaded_applications ()) of
            { value, { App, _, Version } } ->
              ok;
            { value, { App, _, _ } } ->
              ok = application:unload (App),
              ok = erlrc_lib:load_application (App);
            false ->
              ok = erlrc_lib:load_application (App)
          end,
          case lists:keysearch (App, 1, application:loaded_applications ()) of
            { value, { App, _, Version } } ->
              case is_included (App) of
                true ->
                  included;
                false ->
                  % ???: we were getting undef function errors during
                  % package installs, not sure why, thought this might help
                  case application:get_key (App, modules) of
                    { ok, Mods } ->
                      lists:foreach (fun (M) ->
                                       code:ensure_loaded (M)
                                     end,
                                     Mods);
                    undefined ->
                      ok
                  end,
                  case application:get_key (App, included_applications) of
                    { ok, [] } ->
                      ok = application:start (App),
                      started;
                    undefined ->
                      ok = application:start (App),
                      started;
                    { ok, Included } ->
                      lists:foreach
                        (fun (A) ->
                           local_info_msg ("Stopping application ~p, as it is "
					   "now included in application ~p~n",
					   [ A, App ]),
                           ok = maybe_stop (A)
                         end,
                         Included),
                      ok = application:start (App),
                      started_included_stopped
                  end
              end;
            { value, { App, _, _ } } ->
              version_load_mismatch
          end;
        _ ->
          bad_directory
      end
  end.

%% @spec stop (atom(), string())
%%         -> stopped
%%          | stopped_included_started
%%          | version_mismatch
%%          | not_running
%% @doc Stop the specified application version. Does not unload modules
%% or adjust the code path.
%% Returns 'stopped' if the application version was previously running.
%% Returns 'version_mismatch' if a different version of the application
%% is running (use downgrade instead).
%% Returns 'not_running' if the application version was not previously
%% running.
%% Returns 'stopped_included_started' if the application
%% was stopped and any included applications listed in /applications
%% were started successfully.
%% @end

stop (Application, Version) ->
  case lists:keysearch (Application, 1, application:which_applications ()) of
    { value, { Application, _, Version } } ->
      ok = application:stop (Application),
      case application:get_key (Application, included_applications) of
        { ok, [] } ->
          stopped;
        undefined ->
          stopped;
        { ok, Included } ->
          { ok, Root } = application:get_env (erlrc, root_dir),
          lists:foreach (fun (A) ->
                           case should_run (Root, A) of
                             true ->
                               local_info_msg ("Starting application ~p, "
					       "as including application ~p "
					       "has been stopped~n",
					       [ A, Application ]),
                               ok = application:start (A);
                             false ->
                               ok
                           end
                         end,
                         Included),
          stopped_included_started
      end;
    { value, { Application, _, OtherVersion }} ->
      local_error_msg ("erlrcdynamic:stop/2: got OtherVersion '~p' "
                       "which is not Version '~p'~n",
                       [ OtherVersion, Version ]),
      version_mismatch;
    false ->
      not_running
  end.

%% @spec (atom(), string()) -> unloaded | version_load_mismatch | bad_directory
%% @equiv unload (Application, Version, Dir)
%% @doc Unload the application specification for Application from the
%% application controller, purge all of the application's modules,
%% and remove the application's directory from the code path.
%% Assumes the application's directory is in the standard location
%% under code:lib_dir ().
%% @end

unload (Application, Version) ->
  Dir = code:lib_dir (),
  AppDir = Dir ++ "/" ++ atom_to_list (Application) ++ "-" ++ Version,
  unload (Application, Version, AppDir).

%% @spec unload (atom(), string(), string())
%%         -> unloaded | version_load_mismatch | bad_directory
%% @doc Unload the application specification for Application from the
%% application controller, purge all of the application's modules,
%% and remove the application's directory from the code path.
%% Returns 'unloaded' if successful, 'bad_directory' if the given
%% directory does not exist, or 'version_load_mismatch' if a different
%% version of the application was loaded.
%% @end

unload (Application, Version, AppDir) ->
  protect_state (fun () ->
		   unload_unprotected (Application, Version, AppDir)
		 end,
		 fun (R) ->
		   R =:= version_load_mismatch
		 end).

unload_unprotected (App, Version, AppDir) ->
  EbinDir = AppDir ++ "/ebin",
  case code:add_patha (EbinDir) of
    true ->
      case lists:keysearch (App, 1, application:loaded_applications ()) of
        false -> ok = erlrc_lib:load_application (App);
        _     -> ok
      end,
      case lists:keysearch (App, 1, application:loaded_applications ()) of
        { value, { App, _, Version } } ->
          case application:get_key (App, modules) of
            { ok, Mods } ->
              lists:foreach (fun (M) ->
                               code:purge (M),
			       code:delete (M)
                             end,
                             Mods);
            undefined ->
              ok
          end,
	  case application:unload (App) of
	    ok ->
              code:del_path (EbinDir),
              unloaded;
	    Error ->
	      Error
	  end;
        { value, { App, _, OtherVersion } } when OtherVersion =/= Version ->
          version_load_mismatch
      end;
    _ ->
      bad_directory
  end.

%% @spec upgrade (atom(), string(), string())
%%         -> { ok, Reason::atom () }
%%          | { ok, [ Unpurged ] }
%%          | restart_new_emulator
%%          | { error, Reason }
%% @equiv upgrade (Application,
%%                 EarlierVersion,
%%                 LaterVersion,
%%                 EarlierDir,
%%                 LaterDir)
%% @doc Upgrade Application from EarlierVersion to LaterVersion. Generates
%% an .appup file if one does not exist already, and then calls
%% release_handler:upgrade_app/2. The two versions' directories EarlierDir
%% and LaterDir are assumed to be in the standard location under code:lib_dir().
%% @end

upgrade (Application, EarlierVersion, LaterVersion) ->
  Prefix = code:lib_dir () ++ "/" ++ atom_to_list (Application) ++ "-",
  EarlierDir = Prefix ++ EarlierVersion,
  LaterDir = Prefix ++ LaterVersion,
  upgrade (Application, EarlierVersion, LaterVersion, EarlierDir, LaterDir).

%% @spec upgrade (atom(), string(), string(), string(), string())
%%         -> { ok, Reason::atom () }
%%          | { ok, [ Unpurged ] }
%%          | restart_new_emulator
%%          | { error, Reason }
%% @doc Upgrade Application from EarlierVersion in EarlierDir to
%% LaterVersion in LaterDir. Generates an .appup file if one does not
%% exist already, and then calls release_handler:upgrade_app/2.
%% @end

upgrade (Application, EarlierVersion, LaterVersion, EarlierDir, LaterDir) ->
  protect_state (fun () ->
		   upgrade_unprotected (Application,
					EarlierVersion,
					LaterVersion,
					EarlierDir,
					LaterDir)
		 end).

upgrade_unprotected (Application,
		     EarlierVersion,
		     LaterVersion,
		     EarlierDir,
		     LaterDir) ->
  case lists:keysearch (Application, 1, application:which_applications ()) of
    { value, { Application, _, LaterVersion } } ->
      { ok, already_running };
    { value, { Application, _, EarlierVersion } } ->
      do_upgrade (Application,
		  EarlierVersion,
		  LaterVersion,
		  EarlierDir,
		  LaterDir);
    false ->
      case file:read_file_info (EarlierDir) of
        { ok, _ } ->
          case do_upgrade (Application,
                           EarlierVersion,
                           LaterVersion,
                           EarlierDir,
                           LaterDir)
	  of
            R = { ok, _ } ->
              case start (Application, LaterVersion, LaterDir) of
                included -> R;
                already_running -> R;
                started -> R;
                started_included_stopped -> R;
                version_mismatch -> { error, version_mismatch };
                version_load_mismatch -> { error, version_load_mismatch }
              end;
            X ->
              X
          end;
        { error, enoent } ->
          % ok we'll interpret this to mean the old version was never
          % installed.  TODO: is there something better?
          case start (Application, LaterVersion, LaterDir) of
            included -> { ok, included };
            already_running -> { ok, already_running };
            started -> { ok, started };
            started_included_stopped -> { ok, started_included_stopped };
            version_mismatch -> { error, version_mismatch };
            version_load_mismatch -> { error, version_load_mismatch }
          end
      end;
    { value, { Application, _, OtherVersion } } ->
      local_error_msg ("erlrcdynamic:upgrade/5: got OtherVersion '~p' "
                       "which is neither LaterVersion '~p' "
		       "nor EarlierVersion '~p'~n",
                       [ OtherVersion, LaterVersion, EarlierVersion ]),
      { error, version_mismatch }
  end.

%-=====================================================================-
%-                               Private                               -
%-=====================================================================-

beam_exports (Beam, Func, Arity) ->
  case beam_lib:chunks (Beam, [ exports ]) of
    { ok, { _, [ { exports, Exports } ] } } ->
      lists:member ({ Func, Arity }, Exports);
    _ ->
      false
  end.

% included application differences when going from FromDir to ToDir:
%   { Added::list(), Removed::list() }
delta_includes (Application, FromDir, ToDir) ->
  { ok, [ { application, Application, FromProps } ] } =
    file:consult (FromDir ++ "/ebin/" ++ atom_to_list (Application) ++ ".app"),
  { ok, [ { application, Application, ToProps } ] } =
    file:consult (ToDir ++ "/ebin/" ++ atom_to_list (Application) ++ ".app"),

  case { lists:keysearch (included_applications, 1, FromProps),
         lists:keysearch (included_applications, 1, ToProps) }
  of
    { { value, { included_applications, FromInc } },
      { value, { included_applications, ToInc } } } ->
      { ToInc -- FromInc, FromInc -- ToInc };
    { false, { value, { included_applications, ToInc } } } ->
      { ToInc, [] };
    { { value, { included_applications, FromInc } }, false } ->
      { [], FromInc };
    { false, false } ->
      { [], [] }
  end.

do_downgrade (Application,
	      EarlierVersion,
	      LaterVersion,
	      EarlierDir,
	      LaterDir) ->
  case maybe_make_appup (Application,
			 EarlierVersion,
			 LaterVersion,
			 EarlierDir,
			 LaterDir)
  of
    { ok, AppUp } ->
      % work around
      % http://www.erlang.org/pipermail/erlang-bugs/2008-February/000656.html

      case application:get_key (Application, modules) of
        { ok, Mods } ->
          lists:foreach (fun (M) -> code:ensure_loaded (M) end, Mods);
        undefined ->
          ok
      end,

      { AddedIncluded, RemovedIncluded } =
	delta_includes (Application, LaterDir, EarlierDir),
      lists:foreach (fun (A) ->
                       local_info_msg ("Stopping application '~p', as it is "
				       "now included in application '~p'~n",
				       [ A, Application ]),
                       ok = maybe_stop (A)
                     end,
                     AddedIncluded),

      case downgrade_app (Application,
			  AppUp,
			  EarlierVersion,
                          EarlierDir,
			  LaterVersion,
			  LaterDir)
      of
        R = { ok, _ } ->
          { ok, Root } = application:get_env (erlrc, root_dir),
          lists:foreach (fun (A) ->
                           case should_run (Root, A) of
                             true ->
                               local_info_msg ("Starting application '~p', "
					       "as it is no longer included "
					       "in application '~p'~n",
					       [ A, Application ]),
                               ok = application:start (A);
                             false ->
                               ok
                           end
                         end,
                         RemovedIncluded),
          R;
        X ->
          X
      end;
    R = { error, _ } ->
      R
  end.

do_upgrade (Application,
	    EarlierVersion,
	    LaterVersion,
	    EarlierDir,
	    LaterDir) ->
  case maybe_make_appup (Application,
			 EarlierVersion,
			 LaterVersion,
			 EarlierDir,
			 LaterDir)
  of
    { ok, AppUp } ->
      % work around
      % http://www.erlang.org/pipermail/erlang-bugs/2008-February/000656.html

      case application:get_key (Application, modules) of
        { ok, Mods } ->
          lists:foreach (fun (M) -> code:ensure_loaded (M) end, Mods);
        undefined ->
          ok
      end,

      { AddedIncluded, RemovedIncluded } =
	delta_includes (Application, EarlierDir, LaterDir),
      lists:foreach (fun (A) ->
                       local_info_msg ("Stopping application '~p', as it is "
				       "now included in application '~p'~n",
				       [ A, Application ]),
                       ok = maybe_stop (A)
                     end,
                     AddedIncluded),

      case upgrade_app (Application,
			AppUp,
			EarlierVersion,
                        EarlierDir,
			LaterVersion,
			LaterDir)
      of
        R = { ok, _ } ->
          { ok, Root } = application:get_env (erlrc, root_dir),
          lists:foreach (fun (A) ->
                           case should_run (Root, A) of
                             true ->
                               local_info_msg ("Starting application '~p', "
					       "as it is no longer included "
					       "in application '~p' ",
					       [ A, Application ]),
                               ok = application:start (A);
                             false ->
                               ok
                           end
                         end,
                         RemovedIncluded),
          R;
        X ->
          X
      end;
    R = { error, _ } ->
      R
  end.

downgrade_directives (EarlierVersion, LaterVersion, M, Beam) ->
  case is_supervisor (Beam) of
    true ->
      downgrade_directives_supervisor (EarlierVersion, LaterVersion, M, Beam);
    false ->
      case has_code_change (Beam) of
        true  -> [ { update, M, { advanced, [] } } ];
        false -> [ { load_module, M } ]
      end
  end.

downgrade_directives_supervisor (EarlierVersion, LaterVersion, M, Beam) ->
  case beam_exports (Beam, sup_downgrade_notify, 2) of
    true ->
      [ { apply,
	  { M, sup_downgrade_notify, [ EarlierVersion, LaterVersion ] } },
        { update, M, supervisor } ];
    false ->
      [ { update, M, supervisor } ]
  end.

has_code_change (Beam) ->
  beam_exports (Beam, code_change, 3).

has_element (Attr, Key, Elem) ->
  case lists:keysearch (Key, 1, Attr) of
    { value, { Key, Value } } ->
      lists:member (Elem, Value);
    _ ->
      false
  end.

has_version_change (Beam) ->
  beam_exports (Beam, version_change, 2).

is_included (Application) ->
  is_included (Application, application:which_applications ()).

is_included (_, []) ->
  false;
is_included (Application, [ { App, _, _ } | T ]) ->
  case application:get_key (App, included_applications) of
    { ok, Included } ->
      lists:member (Application, Included) orelse is_included (Application, T);
    undefined ->
      is_included (Application, T)
  end.

is_supervisor (Beam) ->
  case beam_lib:chunks (Beam, [ attributes ]) of
    { ok, { _, [ { attributes, Attr } ] } } ->
      has_element (Attr, behaviour, supervisor) orelse
      has_element (Attr, behavior, supervisor);
    _ ->
      false
  end.

local_info_msg (Format, Args) ->
  Leader = erlang:group_leader (),
  try
    true = erlang:group_leader (self (), self ()),
    error_logger:info_msg (Format, Args)
  after
    erlang:group_leader (Leader, self ())
  end.

make_appup (Application, EarlierVersion, LaterVersion, EarlierDir, LaterDir) ->
  case file:consult (EarlierDir ++ "/ebin/" ++
                     atom_to_list (Application) ++ ".app")
  of
    { ok, [ { application, Application, EarlierProps } ] } ->
      case vsn (EarlierProps) =:= EarlierVersion of
        true ->
          case file:consult (LaterDir ++ "/ebin/" ++
                             atom_to_list (Application) ++ ".app")
	  of
            { ok, [ { application, Application, LaterProps } ] } ->
              case vsn (LaterProps) =:= LaterVersion of
                true ->
		  make_appup (Application,
			      EarlierVersion,
			      EarlierProps,
			      LaterVersion,
			      LaterDir,
			      LaterProps);
                false ->
                  { error, bad_new_appvsn }
              end;
            _ ->
	      { error, bad_new_appfile }
          end;
        false ->
          { error, bad_old_appvsn }
      end;
    _ ->
      { error, bad_old_appfile }
  end.

make_appup (Application,
	    EarlierVersion,
	    EarlierProps,
	    LaterVersion,
	    LaterDir,
	    LaterProps) ->
  AddMods = modules (LaterProps) -- modules (EarlierProps),
  DelMods = modules (EarlierProps) -- modules (LaterProps),

  { UpVersionChange, DownVersionChange } =
    case start_module (LaterProps) of
      { ok, StartMod, StartArgs } ->
	StartModBeamFile =
	  LaterDir ++ "/ebin/" ++ atom_to_list (StartMod) ++ ".beam",
	{ [ D
	    || { ok, Beam } <- [ file:read_file (StartModBeamFile) ],
	       D <- version_change (Beam,
				    EarlierVersion,
				    StartMod,
				    StartArgs) ],
	  [ D
	    || { ok, Beam } <- [ file:read_file (StartModBeamFile) ],
	       D <- version_change (Beam,
				    { down, EarlierVersion },
				    StartMod,
				    StartArgs) ] };
      undefined ->
	{ [], [] }
    end,

  UpDirectives =
    [ D
      || M <- modules (LaterProps) -- AddMods,
	 BeamFile <- [ LaterDir ++ "/ebin/" ++ atom_to_list (M) ++ ".beam" ],
	 { ok, Beam } <- [ file:read_file (BeamFile) ],
	 D <- upgrade_directives (EarlierVersion, LaterVersion, M, Beam) ],

  DownDirectives =
    [ D
      || M <- lists:reverse (modules (LaterProps) -- AddMods),
	 BeamFile <- [ LaterDir ++ "/ebin/" ++ atom_to_list (M) ++ ".beam" ],
	 { ok, Beam } <- [ file:read_file (BeamFile) ],
	 D <- downgrade_directives (EarlierVersion, LaterVersion, M, Beam) ],

  AppUp =
    { LaterVersion,
      [ { EarlierVersion,
	  [ { add_module, M } || M <- AddMods ]
	  ++ UpDirectives
	  ++ UpVersionChange
	  ++ [ { delete_module, M } || M <- DelMods ]
	}
      ],
      [ { EarlierVersion,
	  [ { add_module, M } || M <- lists:reverse (DelMods) ]
	  ++ DownVersionChange
	  ++ DownDirectives
	  ++ [ { delete_module, M } || M <- lists:reverse (AddMods) ]
	}
      ]
    },
  local_info_msg ("make_appup/6: generated AppUp for ~p ~p -> ~p~n~p~n",
		  [ Application, EarlierVersion, LaterVersion, AppUp ]),
  { ok, AppUp }.

maybe_make_appup (Application,
		  EarlierVersion,
		  LaterVersion,
		  EarlierDir,
		  LaterDir) ->
  case file:consult (LaterDir ++ "/ebin/" ++
                     atom_to_list (Application) ++ ".appup") of
    { ok, [ AppUp ] } ->
      { ok, AppUp };
    { error, enoent } ->
      make_appup (Application,
		  EarlierVersion,
		  LaterVersion,
		  EarlierDir,
		  LaterDir)
  end.

maybe_stop (Application) ->
  case application:stop (Application) of
    { error, { not_started, Application } } -> ok;
    R -> R
  end.

modules (Props) ->
  { value, { modules, Modules } } = lists:keysearch (modules, 1, Props),
  Modules.

should_run (Root, A) ->
  case file:read_file_info (Root ++ "/applications/" ++ atom_to_list (A)) of
    { ok, _ } -> true;
    _         -> false
  end.

start_module (Props) ->
  case lists:keysearch (mod, 1, Props) of
    { value, { mod, { StartMod, StartArgs } } } ->
      { ok, StartMod, StartArgs };
    false ->
      undefined
  end.

upgrade_directives (EarlierVersion, LaterVersion, M, Beam) ->
  case is_supervisor (Beam) of
    true ->
      upgrade_directives_supervisor (EarlierVersion, LaterVersion, M, Beam);
    false ->
      case has_code_change (Beam) of
        true  -> [ { update, M, { advanced, [] } } ];
        false -> [ { load_module, M } ]
      end
  end.

upgrade_directives_supervisor (EarlierVersion, LaterVersion, M, Beam) ->
  case beam_exports (Beam, sup_upgrade_notify, 2) of
    true ->
      [ { update, M, supervisor },
        { apply,
	  { M, sup_upgrade_notify, [ EarlierVersion, LaterVersion ] } } ];
    false ->
      [ { update, M, supervisor } ]
  end.

version_change (Beam, From, StartMod, StartArgs) ->
  case has_version_change (Beam) of
    true ->
      [ { apply, { StartMod, version_change, [ From, StartArgs ] } } ];
    false ->
      []
  end.

vsn (Props) ->
  { value, { vsn, Vsn } } = lists:keysearch (vsn, 1, Props),
  Vsn.

%-=====================================================================-
%-                     Save and restore code state                     -
%-=====================================================================-

protect_state (F) ->
  % { error, _ } is always considered an error
  protect_state (F, fun (_) -> false end).

protect_state (F, IsError) ->
  State = save_state (),
  Result = try F () of
	     % hmm
	     { 'EXIT', Error } -> { error, Error };
	     R -> R
	   catch
	     _:Error -> { error, Error }
	   end,
  case Result of
    { error, _ } ->
      restore_state (State);
    _ ->
      case IsError (Result) of
	true  -> restore_state (State);
	false -> ok
      end
  end,
  Result.

save_state () ->
  CodePath = code:get_path (),
  LoadedMods = lists:sort (code:all_loaded ()),
  { state, CodePath, LoadedMods }.

restore_state ({ state, OrigCodePath, OrigLoadedMods }) ->
  Path = lists:filter (fun (Dir) ->
			 case file:read_file_info (Dir) of
			   { ok, #file_info { type = directory } } -> true;
			   _ -> false
			 end
		       end,
		       OrigCodePath),
  case code:set_path (Path) of
    true ->
      ok;
    Error ->
      local_error_msg ("Error restoring code path: ~w~n"
		       "Attempted path was: ~p~n",
		       [ Error, Path ])
  end,
  case restore_mods (OrigLoadedMods, lists:sort (code:all_loaded ()), []) of
    [] ->
      ok;
    Errors ->
      local_error_msg ("Errors restoring loaded modules:~n  ~p~n", [Errors])
  end.

% module unchanged
restore_mods ([ { Mod, Path } | OldMods ],
	      [ { Mod, Path } | NewMods ],
	      Actions) ->
  restore_mods (OldMods, NewMods, Actions);

% module path changed
restore_mods ([ { Mod, OldPath } | OldMods ],
	      [ { Mod, NewPath } | NewMods ],
	      Actions)
	when OldPath =/= NewPath ->
  restore_mods (OldMods, NewMods, [ { Mod, { load, OldPath } } | Actions ]);

% module no longer loaded
restore_mods ([ { OldMod, OldPath } | OldMods ],
	      NewMods = [ { NewMod, _NewPath } | _ ],
	      Actions)
	when OldMod < NewMod ->
  restore_mods (OldMods, NewMods, [ { OldMod, { load, OldPath } } | Actions ]);

% module newly loaded, must be unloaded
restore_mods (OldMods = [ { OldMod, _OldPath } | _ ],
	      [ { NewMod, _NewPath } | NewMods ],
	      Actions)
	when OldMod > NewMod ->
  restore_mods (OldMods, NewMods, [ { NewMod, delete } | Actions ]);

% module no longer loaded
restore_mods ([ { OldMod, OldPath } | OldMods ], [], Actions) ->
  restore_mods (OldMods, [], [ { OldMod, { load, OldPath } } | Actions ]);

% module newly loaded, must be unloaded
restore_mods ([], [ { NewMod, _NewPath } | NewMods ], Actions) ->
  restore_mods ([], NewMods, [ { NewMod, delete } | Actions ]);

% perform the collected actions
restore_mods ([], [], Actions) ->
  lists:foldl (fun
		 ({ Mod, delete }, Errors) ->
		    code:purge (Mod),
		    case code:delete (Mod) of
		      true  -> Errors;
		      false -> [ { Mod, delete_failed } | Errors ]
		    end;
		 ({ Mod, { load, Path } }, Errors) ->
		   Ext = code_aux:objfile_extension (),
		   Base = filename:dirname (Path) ++ "/" ++
			  filename:basename (Path, Ext),
		   case code:load_abs (Base) of
		     { module, Mod } -> Errors;
		     Error           -> [ { Mod, Error } | Errors ]
		   end
	       end,
	       [],
	       Actions).

%-=====================================================================-
%-                    Extracted from release_handler                   -
%-                                                                     -
%- Modified to take an appup spec directly, to avoid filesystem        -
%- permissions problems.                                               -
%-=====================================================================-

downgrade_app (App, AppUp, OldVsn, OldDir, NewVsn, NewDir) ->
  try downgrade_script (App, AppUp, OldVsn, OldDir, NewVsn, NewDir) of
    { ok, Script } ->
      release_handler:eval_appup_script (App, OldVsn, OldDir, Script)
  catch
    throw:Reason -> {error, Reason}
  end.

downgrade_script (App, AppUp, OldVsn, OldDir, NewVsn, NewDir) ->
  { NewVsn, Script } = find_script (AppUp, OldVsn, down),
  OldAppl = read_app (App, OldVsn, OldDir),
  NewAppl = read_app (App, NewVsn, NewDir),
  case systools_rc:translate_scripts(dn, [Script], [OldAppl], [NewAppl]) of
    { ok, LowLevelScript }         -> { ok, LowLevelScript };
    { error, _SystoolsRC, Reason } -> throw (Reason)
  end.

upgrade_app(App, AppUp, OldVsn, OldDir, NewVsn, NewDir) ->
  try upgrade_script (App, AppUp, OldVsn, OldDir, NewVsn, NewDir) of
    { ok, NewVsn, Script } ->
      release_handler:eval_appup_script (App, NewVsn, NewDir, Script)
  catch
    throw:Reason -> { error, Reason }
  end.

upgrade_script(App, AppUp, OldVsn, OldDir, NewVsn, NewDir) ->
  { NewVsn, Script } = find_script (AppUp, OldVsn, up),
  OldAppl = read_app (App, OldVsn, OldDir),
  NewAppl = read_app (App, NewVsn, NewDir),
  case systools_rc:translate_scripts (up, [Script], [NewAppl], [OldAppl]) of
    { ok, LowLevelScript }         -> { ok, NewVsn, LowLevelScript };
    { error, _SystoolsRC, Reason } -> throw (Reason)
  end.

find_script (AppUp, OldVsn, UpOrDown) ->
  case AppUp of
    { NewVsn, UpFromScripts, DownToScripts } ->
      Scripts = case UpOrDown of
		  up   -> UpFromScripts;
		  down -> DownToScripts
		end,
      case lists:keysearch (OldVsn, 1, Scripts) of
	{ value, { _OldVsn, Script } } ->
	  { NewVsn, Script };
	false ->
	  throw ({ version_not_in_appup, OldVsn })
      end
  end.

read_app (App, Vsn, Dir) ->
  AppS = atom_to_list (App),
  Path = [ filename:join (Dir, "ebin") ],
  case systools_make:read_application (AppS, Vsn, Path, []) of
    { ok, Appl } ->
      Appl;
    { error, { not_found, _AppFile } } ->
      throw ({ no_app_found, Vsn, Dir });
    { error, Reason } ->
      throw (Reason)
  end.

%-=====================================================================-
%-                                Tests                                -
%-=====================================================================-

-ifdef (EUNIT).

app_setup () ->
  OsPid = os:getpid (),
  Dir = "erlrcmakeappuptest" ++ OsPid,
  os:cmd ("rm -rf " ++ Dir),
  ok = file:make_dir (Dir),

  % ---------------------------------------------------------------------------
  SupervisorErl = <<"
-module (erlrctestmakeappupsup).
-behavior (supervisor).
-export ([ start_link/0, init/1 ]).
init ([]) -> { ok, { { one_for_one, 3, 10 }, [ { erlrctestmakeappupsrv, { erlrctestmakeappupsrv, start_link, [ ] }, temporary, 10000, worker, [ erlrctestmakeappupsrv ] } ] } }.
start_link () -> supervisor:start_link (?MODULE, []).
">>,
  % ---------------------------------------------------------------------------

  ErlFiles = [
    % -------------------------------------------------------------------------
    { "/sup-inc/ebin/erlrctestmakeappupinc.erl",
      <<"
-module (erlrctestmakeappupinc).
-vsn (\"0.0.0\").
-behaviour (application).
-export ([ start/2, stop/1, version_change/2 ]).

start (Type, Args) -> erlrctestmakeappup:start (Type, Args).
stop (Arg) -> erlrctestmakeappup:stop (Arg).

version_change (_From, _Extra) ->
  case whereis (erlrctestmakeappupsrv) of
    Pid when is_pid (Pid) ->
      error_logger:info_msg (\"killing erlrctestmakeappupsrv ~p~n\", [Pid]),
      MRef = erlang:monitor (process, Pid),
      exit (Pid, shutdown),
      receive { 'DOWN', MRef, _, _, _ } -> ok end,
      ok;
    _ ->
      error_logger:info_msg (\"no erlrctestmakeappupsrv found~n\", []),
      ok
  end.
">> },
    % -------------------------------------------------------------------------
    { "/sup-incnew/ebin/erlrctestmakeappupinc.erl",
      <<"
-module (erlrctestmakeappupinc).
-vsn (\"0.0.1\").
-behaviour (application).
-export ([ start/2, stop/1, version_change/2 ]).

start (_Type, _Args) -> { ok, spawn_link (fun () -> receive after infinity -> ok end end) }.
stop (_Arg) -> ok.

version_change (_From, _Extra) ->
  case whereis (erlrctestmakeappupsrv) of
    Pid when is_pid (Pid) ->
      error_logger:info_msg (\"killing erlrctestmakeappupsrv ~p~n\", [ Pid ]),
      MRef = erlang:monitor (process, Pid),
      exit (Pid, shutdown),
      receive { 'DOWN', MRef, _, _, _ } -> ok end,
      ok;
    _ ->
      error_logger:info_msg (\"no erlrctestmakeappupsrv found~n\", [ ]),
      ok
  end.
">> },
    % -------------------------------------------------------------------------
    { "/sup-old/ebin/erlrctestmakeappup.erl",
      <<"
-module (erlrctestmakeappup).
-vsn (\"0.0.0\").
-behaviour (application).
-export ([ start/2, stop/1 ]).

start (_Type, _Args) -> erlrctestmakeappupsup:start_link ().
stop (_) -> ok.
">> },
    % -------------------------------------------------------------------------
    { "/sup-old/ebin/erlrctestmakeappupsup.erl", SupervisorErl },
    % -------------------------------------------------------------------------
    { "/sup-old/ebin/erlrctestmakeappupsrv.erl",
      <<"
-module (erlrctestmakeappupsrv).
-vsn (\"0.0.0\").
-export ([ start_link/0, get/0 ]).
-export ([ init/1,
           handle_call/3,
           handle_cast/2,
           handle_info/2,
           terminate/2,
           code_change/3 ]).

start_link () -> gen_server:start_link ({ local, ?MODULE }, ?MODULE, [], []).
get () -> gen_server:call (?MODULE, get).
init ([]) -> { ok, { \"0.0.0\", init } }.
handle_call (get, _, State) -> { reply, State, State }.
handle_cast (_, State) -> { noreply, State }.
handle_info (_, State) -> { noreply, State }.
terminate (_, _) -> ok.
code_change (OldVsn, State, Extra) -> { ok, { \"0.0.0\", { code_change, OldVsn } } }.
">> },
    % -------------------------------------------------------------------------
    { "/sup-new/ebin/erlrctestmakeappup.erl",
      <<"
-module (erlrctestmakeappup).
-behaviour (application).
-export ([ start/2, stop/1 ]).
-export ([ flass/0 ]).

start (_Type, _Args) -> erlrctestmakeappupsup:start_link ().
stop (_) -> ok.
flass () -> turg.
">> },
    % -------------------------------------------------------------------------
    { "/sup-new/ebin/erlrctestmakeappupsup.erl", SupervisorErl },
    % -------------------------------------------------------------------------
    { "/sup-new/ebin/erlrctestmakeappupsrv.erl",
      <<"
-module (erlrctestmakeappupsrv).
-vsn (\"0.0.1\").
-export ([ start_link/0, get/0 ]).
-export ([ init/1,
           handle_call/3,
           handle_cast/2,
           handle_info/2,
           terminate/2,
           code_change/3 ]).

start_link () -> gen_server:start_link ({ local, ?MODULE }, ?MODULE, [], []).
get () -> gen_server:call (?MODULE, get).
init ([]) -> { ok, { \"0.0.1\", init } }.
handle_call (get, _, State) -> { reply, State, State }.
handle_cast (_, State) -> { noreply, State }.
handle_info (_, State) -> { noreply, State }.
terminate (_, _) -> ok.
code_change (OldVsn, State, Extra) -> { ok, { \"0.0.1\", { code_change, OldVsn } } }.
">> },
    % -------------------------------------------------------------------------
    { "/sup-new/ebin/erlrctestmakeappupdild.erl",
      <<"-module (erlrctestmakeappupdild).">> },
    % -------------------------------------------------------------------------
    { "/sup-newnew/ebin/erlrctestmakeappup.erl",
      <<"
-module (erlrctestmakeappup).
-vsn (\"0.0.2\").
-behaviour (application).
-export ([ start/2, stop/1 ]).

start (_Type, _Args) -> erlrctestmakeappupsup:start_link ().
stop (_) -> ok.
 ">> },
    % -------------------------------------------------------------------------
    { "/sup-newnew/ebin/erlrctestmakeappupsup.erl", SupervisorErl },
    % -------------------------------------------------------------------------
    { "/sup-newnew/ebin/erlrctestmakeappupsrv.erl",
      <<"
-module (erlrctestmakeappupsrv).
-vsn (\"0.0.2\").
-export ([ start_link/0, get/0 ]).
-export ([ init/1,
           handle_call/3,
           handle_cast/2,
           handle_info/2,
           terminate/2,
           code_change/3 ]).

start_link () -> gen_server:start_link ({ local, ?MODULE }, ?MODULE, [], []).
get () -> gen_server:call (?MODULE, get).
init ([]) -> { ok, { \"0.0.2\", init } }.
handle_call (get, _, State) -> { reply, State, State }.
handle_cast (_, State) -> { noreply, State }.
handle_info (_, State) -> { noreply, State }.
terminate (_, _) -> ok.
code_change (OldVsn, State, Extra) -> { ok, { \"0.0.2\", { code_change, OldVsn } } }.
">> },
    % -------------------------------------------------------------------------
    { "/sup-newnew/ebin/erlrctestmakeappupdild.erl",
      <<"-module (erlrctestmakeappupdild).">> },
    % -------------------------------------------------------------------------
    { "/borken-old/ebin/borken.erl",
      <<"
-module (borken).
-vsn (\"0.0.0\").
-behaviour (application).
-export ([ start/2, stop/1 ]).

start (_Type, _Args) -> borkensup:start_link ().
stop (_) -> ok.
">> },
    % -------------------------------------------------------------------------
    { "/borken-old/ebin/borkensup.erl",
      <<"
-module (borkensup).
-vsn (\"0.0.0\").
-behavior (supervisor).
-export ([ start_link/0, init/1 ]).

start_link () -> supervisor:start_link (?MODULE, []).
init ([]) -> { ok, { { one_for_one, 3, 10 }, [ { borkensrv, { borkensrv, start_link, [ ] }, temporary, 10000, worker, [ borkensrv ] } ] } }.
">> },
    % -------------------------------------------------------------------------
    { "/borken-old/ebin/borkensrv.erl",
      <<"
-module (borkensrv).
-vsn (\"0.0.0\").
-export ([ start_link/0, get/0 ]).
-export ([ init/1,
           handle_call/3,
           handle_cast/2,
           handle_info/2,
           terminate/2,
           code_change/3 ]).

start_link () -> gen_server:start_link ({ local, ?MODULE }, ?MODULE, [], []).
get () -> gen_server:call (?MODULE, get).
init ([]) -> { ok, { \"0.0.0\", init } }.
handle_call (get, _, State) -> { reply, State, State }.
handle_cast (_, State) -> { noreply, State }.
handle_info (_, State) -> { noreply, State }.
terminate (_, _) -> ok.
code_change (OldVsn, State, Extra) -> { ok, State }.
">> },
    % -------------------------------------------------------------------------
    { "/borken-new/ebin/borken.erl",
      <<"
-module (borken).
-vsn (\"0.0.1\").
-behaviour (application).
-export ([ start/2, stop/1 ]).

start (_Type, _Args) -> borkensup:start_link ().
stop (_) -> ok.
">> },
    % -------------------------------------------------------------------------
    { "/borken-new/ebin/borkensup.erl",
      <<"
-module (borkensup).
-vsn (\"0.0.1\").
-behavior (supervisor).
-export ([ start_link/0, init/1 ]).

start_link () -> supervisor:start_link (?MODULE, []).
init ([]) -> { ok, { { one_for_one, 3, 10 }, [ { borkensrv, { borkensrv, start_link, [ ] }, temporary, 10000, worker, [ borkensrv ] } ] } }.
">> },
    % -------------------------------------------------------------------------
    { "/borken-new/ebin/borkensrv.erl",
      <<"
-module (borkensrv).
-vsn (\"0.0.1\").
-export ([ start_link/0, get/0 ]).
-export ([ init/1,
           handle_call/3,
           handle_cast/2,
           handle_info/2,
           terminate/2,
           code_change/3 ]).

start_link () -> gen_server:start_link ({ local, ?MODULE }, ?MODULE, [], []).
get () -> gen_server:call (?MODULE, get).
init ([]) -> { ok, { \"0.0.1\", init } }.
handle_call (get, _, State) -> { reply, State, State }.
handle_cast (_, State) -> { noreply, State }.
handle_info (_, State) -> { noreply, State }.
terminate (_, _) -> ok.
%code_change (OldVsn, State, Extra) -> erlang:error (borken).
code_change (OldVsn, State, Extra) -> borken.
">> }
    % -------------------------------------------------------------------------
  ],
  lists:foreach (fun ({ ErlPath, Binary }) ->
		   ErlFile = Dir ++ ErlPath,
		   ok = filelib:ensure_dir (ErlFile),
		   SubDir = filename:dirname (ErlFile),
		   ok = file:write_file (ErlFile, Binary),
		   "ok" = os:cmd ("cd " ++ SubDir ++
				  " && erlc -W0 " ++
				    filename:basename (ErlFile) ++
				  " && printf ok"),
		   BeamFile = SubDir ++ "/" ++
			      filename:basename (ErlFile, ".erl") ++ ".beam",
		   { ok, _ } = file:read_file_info (BeamFile)
		 end,
		 ErlFiles),

  AppFiles = [
    { "/sup-inc/ebin/supinc.app",
      { application,
	supinc,
	[ { vsn, "0.0.0" },
	  { description, "yo" },
	  { registered, [ ] },
	  { applications, [ kernel, stdlib ] },
	  { modules, [ erlrctestmakeappupinc ] },
	  { included_applications, [ sup ] },
	  { mod, { erlrctestmakeappupinc, [] } }
	] }
    },
    { "/sup-incnew/ebin/supinc.app",
      { application,
	supinc,
	[ { vsn, "0.0.1" },
	  { description, "yo" },
	  { registered, [ ] },
	  { applications, [ kernel, stdlib ] },
	  { modules, [ erlrctestmakeappupinc ] },
	  { included_applications, [ ] },
	  { mod, { erlrctestmakeappupinc, [] } }
	] }
    },
    { "/sup-old/ebin/sup.app",
      { application,
	sup,
	[ { vsn, "0.0.0" },
	  { description, "yo" },
	  { registered, [ erlrctestmakeappupsup, erlrctestmakeappupsrv ] },
	  { applications, [ kernel, stdlib ] },
	  { modules, [ erlrctestmakeappupsup,
		       erlrctestmakeappupsrv,
		       erlrctestmakeappup ] },
	  { mod, { erlrctestmakeappup, [] } }
	] }
    },
    { "/sup-new/ebin/sup.app",
      { application,
	sup,
	[ { vsn, "0.0.1" },
	  { description, "yo" },
	  { registered, [ erlrctestmakeappupsup, erlrctestmakeappupsrv ] },
	  { applications, [ kernel, stdlib ] },
	  { modules, [ erlrctestmakeappup, erlrctestmakeappupsup,
		       erlrctestmakeappupsrv, erlrctestmakeappupdild ] },
	  { mod, { erlrctestmakeappup, [] } }
	] }
    },
    { "/sup-newnew/ebin/sup.app",
      { application,
	sup,
	[ { vsn, "0.0.2" },
	  { description, "yo" },
	  { registered, [ erlrctestmakeappupsup, erlrctestmakeappupsrv ] },
	  { applications, [ kernel, stdlib ] },
	  { modules, [ erlrctestmakeappup, erlrctestmakeappupsup,
		              erlrctestmakeappupsrv, erlrctestmakeappupdild ] },
	  { mod, { erlrctestmakeappup, [] } }
	] }
    },
    { "/borken-old/ebin/borken.app",
      { application,
	borken,
	[ { vsn, "0.0.0" },
	  { description, "borken" },
	  { registered, [ borkensup, borkensrv ] },
	  { applications, [ kernel, stdlib ] },
	  { modules, [ borken, borkensup, borkensrv ] },
	  { mod, { borken, [] } }
	] }
    },
    { "/borken-new/ebin/borken.app",
      { application,
	borken,
	[ { vsn, "0.0.1" },
	  { description, "borken" },
	  { registered, [ borkensup, borkensrv ] },
	  { applications, [ kernel, stdlib ] },
	  { modules, [ borken, borkensup, borkensrv ] },
	  { mod, { borken, [] } }
	] }
    }
  ],
  lists:foreach (fun ({ AppPath, Spec }) ->
		   B = erlang:iolist_to_binary (io_lib:format ("~p.", [Spec])),
		   ok = file:write_file (Dir ++ AppPath, B)
		 end,
		 AppFiles),

  ok = file:make_dir (Dir ++ "/erlrc.d"),
  ok = file:make_dir (Dir ++ "/erlrc.d/applications"),
  ok = file:make_dir (Dir ++ "/erlrc.d/nodes"),
  ok = application:set_env (erlrc, root_dir, Dir ++ "/erlrc.d"),

  Dir.

app_teardown (Dir) ->
  application:stop (supinc),
  application:unload (supinc),
  application:stop (sup),
  application:unload (sup),
  application:stop (borken),
  application:unload (borken),
  lists:foreach (fun (M) ->
		   code:purge (M),
		   code:delete (M)
		 end,
                 [ erlrctestmakeappup,
		   erlrctestmakeappupsup,
                   erlrctestmakeappupsrv,
		   erlrctestmakeappupdild,
		   erlrctestmakeappupinc,
		   borken,
		   borkensup,
		   borkensrv ]),
  code:del_path (Dir ++ "/sup-incnew/ebin"),
  code:del_path (Dir ++ "/sup-inc/ebin"),
  code:del_path (Dir ++ "/sup-old/ebin"),
  code:del_path (Dir ++ "/sup-new/ebin"),
  code:del_path (Dir ++ "/borken-old/ebin"),
  code:del_path (Dir ++ "/borken-new/ebin"),
  os:cmd ("rm -rf " ++ Dir),
  ok.

make_appup_test_ () ->
  { setup,
    fun app_setup/0,
    fun app_teardown/1,
    fun () ->
      OsPid = os:getpid (),
      Dir = "erlrcmakeappuptest" ++ OsPid,

      { ok, Appup } = make_appup (sup,
                                  "0.0.0",
                                  "0.0.1",
                                  Dir ++ "/sup-old",
                                  Dir ++ "/sup-new"),

      Appup = { "0.0.1",
                [ { "0.0.0",
                    [ { add_module, erlrctestmakeappupdild },
                      { load_module, erlrctestmakeappup },
                      { update, erlrctestmakeappupsup, supervisor },
                      { update, erlrctestmakeappupsrv, { advanced, [] } } ]
                  } ],
                [ { "0.0.0",
                    [ { update, erlrctestmakeappupsrv, { advanced, [] } },
                      { update, erlrctestmakeappupsup, supervisor },
                      { load_module, erlrctestmakeappup },
                      { delete_module, erlrctestmakeappupdild } ]
                  } ]
              },

      ok
    end }.

start_test_ () ->
  { setup,
    fun app_setup/0,
    fun app_teardown/1,
    fun () ->
      OsPid = os:getpid (),
      Dir = "erlrcmakeappuptest" ++ OsPid,
      version_load_mismatch = start (sup, "0.0.1", Dir ++ "/sup-old"),
      started = start (sup, "0.0.0", Dir ++ "/sup-old"),
      already_running = start (sup, "0.0.0", Dir ++ "/sup-old"),
      version_mismatch = start (sup, "0.0.1", Dir ++ "/sup-old"),
      ok
    end }.

start_included_test_ () ->
  { setup,
    fun app_setup/0,
    fun app_teardown/1,
    fun () ->
      OsPid = os:getpid (),
      Dir = "erlrcmakeappuptest" ++ OsPid,
      false = lists:keymember (supinc, 1, application:which_applications ()),
      false = lists:keymember (sup, 1, application:which_applications ()),
      started = start (sup, "0.0.0", Dir ++ "/sup-old"),
      false = lists:keymember (supinc, 1, application:which_applications ()),
      true = lists:keymember (sup, 1, application:which_applications ()),
      started_included_stopped = start (supinc, "0.0.0", Dir ++ "/sup-inc"),
      true = lists:keymember (supinc, 1, application:which_applications ()),
      false = lists:keymember (sup, 1, application:which_applications ()),
      included = start (sup, "0.0.0", Dir ++ "/sup-old"),
      true = lists:keymember (supinc, 1, application:which_applications ()),
      false = lists:keymember (sup, 1, application:which_applications ()),
      ok
    end }.

stop_test_ () ->
  { setup,
    fun app_setup/0,
    fun app_teardown/1,
    fun () ->
      OsPid = os:getpid (),
      Dir = "erlrcmakeappuptest" ++ OsPid,
      not_running = stop (sup, "0.0.1"),
      not_running = stop (sup, "0.0.0"),
      started = start (sup, "0.0.0", Dir ++ "/sup-old"),
      version_mismatch = stop (sup, "0.0.1"),
      stopped = stop (sup, "0.0.0"),
      not_running = stop (sup, "0.0.0")
    end }.

stop_included_test_ () ->
  { setup,
    fun app_setup/0,
    fun app_teardown/1,
    fun () ->
      OsPid = os:getpid (),
      Dir = "erlrcmakeappuptest" ++ OsPid,
      started = start (sup, "0.0.0", Dir ++ "/sup-old"),
      started_included_stopped = start (supinc, "0.0.0", Dir ++ "/sup-inc"),
      true = lists:keymember (supinc, 1, application:which_applications ()),
      false = lists:keymember (sup, 1, application:which_applications ()),
      stopped_included_started = stop (supinc, "0.0.0"),
      false = lists:keymember (supinc, 1, application:which_applications ()),
      false  = lists:keymember (sup, 1, application:which_applications ()),
      ok = file:write_file (Dir ++ "/erlrc.d/applications/sup", <<>>),
      started_included_stopped = start (supinc, "0.0.0", Dir ++ "/sup-inc"),
      true = lists:keymember (supinc, 1, application:which_applications ()),
      false = lists:keymember (sup, 1, application:which_applications ()),
      stopped_included_started = stop (supinc, "0.0.0"),
      false = lists:keymember (supinc, 1, application:which_applications ()),
      true  = lists:keymember (sup, 1, application:which_applications ()),
      stopped = stop (sup, "0.0.0")
    end }.

unload_test_ () ->
  { setup,
    fun app_setup/0,
    fun app_teardown/1,
    fun () ->
      OsPid = os:getpid (),
      Dir = "erlrcmakeappuptest" ++ OsPid,
      false = code:is_loaded (erlrctestmakeappup),
      version_load_mismatch = unload (sup, "0.0.1", Dir ++ "/sup-old"),
      started = start (sup, "0.0.0", Dir ++ "/sup-old"),
      { file, _ } = code:is_loaded (erlrctestmakeappup),
      stopped = stop (sup, "0.0.0"),
      { file, _ } = code:is_loaded (erlrctestmakeappup),
      unloaded = unload (sup, "0.0.0", Dir ++ "/sup-old"),
      false = code:is_loaded (erlrctestmakeappup),
      unloaded = unload (sup, "0.0.0", Dir ++ "/sup-old"),
      false = code:is_loaded (erlrctestmakeappup),
      false = lists:keysearch (sup, 1, application:loaded_applications ()),
      ok
    end
  }.

upgrade_test_ () ->
  { setup,
    fun app_setup/0,
    fun app_teardown/1,
    fun () ->
      OsPid = os:getpid (),
      Dir = "erlrcmakeappuptest" ++ OsPid,
      started = start (sup, "0.0.0", Dir ++ "/sup-old"),
      { value, { sup, _, "0.0.0" } } =
        lists:keysearch (sup, 1, application:which_applications ()),
      { "0.0.0", init } = erlrctestmakeappupsrv:get (),
      { ok, _ } = erlrcdynamic:upgrade (sup,
					"0.0.0",
					"0.0.1",
					Dir ++ "/sup-old",
					Dir ++ "/sup-new"),
      { value, { sup, _, "0.0.1" } } =
        lists:keysearch (sup, 1, application:which_applications ()),
      { "0.0.1", { code_change, "0.0.0" } } = erlrctestmakeappupsrv:get (),
      ok
    end
  }.

upgrade_existing_appup_test_ () ->
  { setup,
    fun app_setup/0,
    fun app_teardown/1,
    fun () ->
      OsPid = os:getpid (),
      Dir = "erlrcmakeappuptest" ++ OsPid,
      started = start (sup, "0.0.0", Dir ++ "/sup-old"),
      { value, { sup, _, "0.0.0" } } =
        lists:keysearch (sup, 1, application:which_applications ()),
      { "0.0.0", init } = erlrctestmakeappupsrv:get (),
      AppUp = <<"
{\"0.0.1\",
 [{\"0.0.0\",
   [{add_module,erlrctestmakeappupdild},
    {load_module,erlrctestmakeappup},
    {update,erlrctestmakeappupsup,supervisor},
    {load_module,erlrctestmakeappupsrv}]}],
 [{\"0.0.0\",
   [{update,erlrctestmakeappupsrv,{advanced,[]}},
    {update,erlrctestmakeappupsup,supervisor},
    {load_module,erlrctestmakeappup},
    {delete_module,erlrctestmakeappupdild}]}]}.">>,
      ok = file:write_file (Dir ++ "/sup-new/ebin/sup.appup", AppUp),
      { ok, _ } = upgrade (sup,
                           "0.0.0",
                           "0.0.1",
                           Dir ++ "/sup-old",
                           Dir ++ "/sup-new"),
      { value, { sup, _, "0.0.1" } } =
        lists:keysearch (sup, 1, application:which_applications ()),
      % i.e., the custom appup script does not call code_change
      { "0.0.0", init } = erlrctestmakeappupsrv:get (),
      ok
    end }.

upgraded_not_started_test_ () ->
  { setup,
    fun app_setup/0,
    fun app_teardown/1,
    fun () ->
      OsPid = os:getpid (),
      Dir = "erlrcmakeappuptest" ++ OsPid,

      started = start (sup, "0.0.0", Dir ++ "/sup-old"),
      false = erlang:function_exported (erlrctestmakeappup, flass, 0),
      stopped = stop (sup, "0.0.0"),
      code:ensure_loaded (erlrctestmakeappup),
      false = erlang:function_exported (erlrctestmakeappup, flass, 0),
      { ok, _ } = upgrade (sup,
                           "0.0.0",
                           "0.0.1",
                           Dir ++ "/sup-old",
                           Dir ++ "/sup-new"),
      true = erlang:function_exported (erlrctestmakeappup, flass, 0),
      ok
    end }.

downgraded_not_started_test_ () ->
  { setup,
    fun app_setup/0,
    fun app_teardown/1,
    fun () ->
      OsPid = os:getpid (),
      Dir = "erlrcmakeappuptest" ++ OsPid,

      started = start (sup, "0.0.1", Dir ++ "/sup-new"),
      true = erlang:function_exported (erlrctestmakeappup, flass, 0),
      stopped = stop (sup, "0.0.1"),
      code:ensure_loaded (erlrctestmakeappup),
      true = erlang:function_exported (erlrctestmakeappup, flass, 0),
      { ok, _ } = downgrade (sup,
                             "0.0.0",
                             "0.0.1",
                             Dir ++ "/sup-old",
                             Dir ++ "/sup-new"),
      false = erlang:function_exported (erlrctestmakeappup, flass, 0),
      ok
    end }.

upgrade_double_test_ () ->
  { setup,
    fun app_setup/0,
    fun app_teardown/1,
    fun () ->
      OsPid = os:getpid (),
      Dir = "erlrcmakeappuptest" ++ OsPid,
      started = start (sup, "0.0.0", Dir ++ "/sup-old"),
      { value, { sup, _, "0.0.0" } } =
        lists:keysearch (sup, 1, application:which_applications ()),
      { "0.0.0", init } = erlrctestmakeappupsrv:get (),
      { ok, _ } = upgrade (sup,
                           "0.0.0",
                           "0.0.1",
                           Dir ++ "/sup-old",
                           Dir ++ "/sup-new"),
      { value, { sup, _, "0.0.1" } } =
        lists:keysearch (sup, 1, application:which_applications ()),
      { "0.0.1", { code_change, "0.0.0" } } = erlrctestmakeappupsrv:get (),
      { ok, _ } = upgrade (sup,
                           "0.0.1",
                           "0.0.2",
                           Dir ++ "/sup-new",
                           Dir ++ "/sup-newnew"),
      { value, { sup, _, "0.0.2" } } =
        lists:keysearch (sup, 1, application:which_applications ()),
      { "0.0.2", { code_change, "0.0.1" } } = erlrctestmakeappupsrv:get (),
      ok
    end }.

upgrade_included_test_ () ->
  { setup,
    fun app_setup/0,
    fun app_teardown/1,
    fun () ->
      OsPid = os:getpid (),
      Dir = "erlrcmakeappuptest" ++ OsPid,
      ok = file:write_file (Dir ++ "/erlrc.d/applications/sup", <<>>),
      started = start (sup, "0.0.0", Dir ++ "/sup-old"),
      started_included_stopped = start (supinc, "0.0.0", Dir ++ "/sup-inc"),
      true = lists:keymember (supinc, 1, application:which_applications ()),
      false = lists:keymember (sup, 1, application:which_applications ()),
      { "0.0.0", init } = erlrctestmakeappupsrv:get (),
      { ok, _ } = upgrade (sup,
                           "0.0.0",
                           "0.0.1",
                           Dir ++ "/sup-old",
                           Dir ++ "/sup-new"),
      { "0.0.1", { code_change, "0.0.0" } } = erlrctestmakeappupsrv:get (),
      true = lists:keymember (supinc, 1, application:which_applications ()),
      false = lists:keymember (sup, 1, application:which_applications ()),
      ok
    end }.

upgrade_including_test_ () ->
  { setup,
    fun app_setup/0,
    fun app_teardown/1,
    fun () ->
      OsPid = os:getpid (),
      Dir = "erlrcmakeappuptest" ++ OsPid,
      ok = file:write_file (Dir ++ "/erlrc.d/applications/sup", <<>>),
      started = start (sup, "0.0.0", Dir ++ "/sup-old"),
      started_included_stopped = start (supinc, "0.0.0", Dir ++ "/sup-inc"),
      true = lists:keymember (supinc, 1, application:which_applications ()),
      false = lists:keymember (sup, 1, application:which_applications ()),
      { ok, _ } = upgrade (supinc,
                           "0.0.0",
                           "0.0.1",
                           Dir ++ "/sup-inc",
                           Dir ++ "/sup-incnew"),
      true = lists:keymember (supinc, 1, application:which_applications ()),
      true = lists:keymember (sup, 1, application:which_applications ()),
      stopped = stop (supinc, "0.0.1"),
      unloaded = unload (supinc, "0.0.1", Dir ++ "/sup-incnew"),
      stopped = stop (sup, "0.0.0"),
      unloaded = unload (sup, "0.0.0", Dir ++ "/sup-old"),
      started = start (sup, "0.0.0", Dir ++ "/sup-old"),
      started = start (supinc, "0.0.1", Dir ++ "/sup-incnew"),
      true = lists:keymember (supinc, 1, application:which_applications ()),
      true = lists:keymember (sup, 1, application:which_applications ()),
      { ok, _ } = upgrade (supinc,
                           "0.0.1",
                           "0.0.0",
                           Dir ++ "/sup-incnew",
                           Dir ++ "/sup-inc"),
      true = lists:keymember (supinc, 1, application:which_applications ()),
      false = lists:keymember (sup, 1, application:which_applications ()),
      ok
    end }.

downgrade_test_ () ->
  { setup,
    fun app_setup/0,
    fun app_teardown/1,
    fun () ->
      OsPid = os:getpid (),
      Dir = "erlrcmakeappuptest" ++ OsPid,
      started = start (sup, "0.0.1", Dir ++ "/sup-new"),
      { value, { sup, _, "0.0.1" } } =
        lists:keysearch (sup, 1, application:which_applications ()),
      { "0.0.1", init } = erlrctestmakeappupsrv:get (),
      { ok, _ } = downgrade (sup,
                             "0.0.0",
                             "0.0.1",
                             Dir ++ "/sup-old",
                             Dir ++ "/sup-new"),
      { "0.0.1", { code_change, { down, "0.0.0" } } } =
	erlrctestmakeappupsrv:get (),
      { value, { sup, _, "0.0.0" } } =
        lists:keysearch (sup, 1, application:which_applications ()),
      ok
    end }.

downgrade_included_test_ () ->
  { setup,
    fun app_setup/0,
    fun app_teardown/1,
    fun () ->
      OsPid = os:getpid (),
      Dir = "erlrcmakeappuptest" ++ OsPid,
      ok = file:write_file (Dir ++ "/erlrc.d/applications/sup", <<>>),
      started = start (sup, "0.0.1", Dir ++ "/sup-new"),
      started_included_stopped = start (supinc, "0.0.0", Dir ++ "/sup-inc"),
      true = lists:keymember (supinc, 1, application:which_applications ()),
      false = lists:keymember (sup, 1, application:which_applications ()),
      { "0.0.1", init } = erlrctestmakeappupsrv:get (),
      { ok, _ } = downgrade (sup,
                             "0.0.0",
                             "0.0.1",
                             Dir ++ "/sup-old",
                             Dir ++ "/sup-new"),
      { "0.0.1", { code_change, { down, "0.0.0" } } } =
	erlrctestmakeappupsrv:get (),
      true = lists:keymember (supinc, 1, application:which_applications ()),
      false = lists:keymember (sup, 1, application:which_applications ()),
      ok
    end }.

downgrade_including_test_ () ->
  { setup,
    fun app_setup/0,
    fun app_teardown/1,
    fun () ->
      OsPid = os:getpid (),
      Dir = "erlrcmakeappuptest" ++ OsPid,
      ok = file:write_file (Dir ++ "/erlrc.d/applications/sup", <<>>),
      started = start (sup, "0.0.0", Dir ++ "/sup-old"),
      started = start (supinc, "0.0.1", Dir ++ "/sup-incnew"),
      true = lists:keymember (supinc, 1, application:which_applications ()),
      true = lists:keymember (sup, 1, application:which_applications ()),
      { ok, _ } = downgrade (supinc,
                             "0.0.0",
                             "0.0.1",
                             Dir ++ "/sup-inc",
                             Dir ++ "/sup-incnew"),
      true = lists:keymember (supinc, 1, application:which_applications ()),
      false = lists:keymember (sup, 1, application:which_applications ()),
      stopped_included_started = stop (supinc, "0.0.0"),
      stopped = stop (sup, "0.0.0"),
      started_included_stopped = start (supinc, "0.0.0", Dir ++ "/sup-inc"),
      true = lists:keymember (supinc, 1, application:which_applications ()),
      false = lists:keymember (sup, 1, application:which_applications ()),
      { ok, _ } = downgrade (supinc,
                             "0.0.1",
                             "0.0.0",
                             Dir ++ "/sup-incnew",
                             Dir ++ "/sup-inc"),
      true = lists:keymember (supinc, 1, application:which_applications ()),
      true = lists:keymember (sup, 1, application:which_applications ()),
      stopped = stop (supinc, "0.0.1"),
      stopped = stop (sup, "0.0.0"),
      ok
    end }.

borken_upgrade_test_ () ->
  { setup,
    fun app_setup/0,
    fun app_teardown/1,
    fun () ->
      OsPid = os:getpid (),
      Dir = "erlrcmakeappuptest" ++ OsPid,
      started = start (borken, "0.0.0", Dir ++ "/borken-old"),
      { value, { borken, _, "0.0.0" } } =
        lists:keysearch (borken, 1, application:which_applications ()),
      { "0.0.0", init } = borkensrv:get (),
      State0 = save_state (),
      { error, _ } = erlrcdynamic:upgrade (borken,
					   "0.0.0",
					   "0.0.1",
					   Dir ++ "/borken-old",
					   Dir ++ "/borken-new"),
      State1 = save_state (),
      true = (State1 =:= State0),
      ok
    end
  }.

-endif.
