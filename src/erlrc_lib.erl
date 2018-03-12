-module (erlrc_lib).

-export ([ load_application/1,
           get_apps_dir/0,
           load_resource_file/2 ]).

%% @spec load_application (App::atom()) -> ok | { error, Reason }
%% @doc We're having problems with this, so for now this is equivalent
%% to application:load/1.  Here's what we'd like it to do eventually:
%% Load the given application App, obeying an override resource
%% file App.app in the directory $ERLRC_ROOT/applications.
%% for override spec of:
%% <code>
%% { application,
%%   my_app,
%%   [ { modules, [ module, module2 ] }
%%     { env, { keymerge, [ { var, "value" } ] } }
%%     { applications, { merge, [ kernel, stdlib } }
%%     { registered, { override, [ reg, reg2 ] } }
%%   ]
%% }
%% </code>
%% <ul>
%% <li>replaces modules list with the [ module, module2 ]</li>
%% <li>merges the new { var, "value" } into the existing env list,
%%     replacing any { var, _ }</li>
%% <li>merges kernel and stdlib into the applications list.</li>
%% <li>replaces registered list with [ reg, reg2 ]</li>
%% <li>All other props from the override spec remain.</li>
%% </ul>
%% @end

load_application (erlrc) ->
  % Special case: code below depends on erlrc's application spec being
  % loaded, but of course load_application (erlrc) implies it isn't.
  % So, there can be no overrides of erlrc's application spec.
  application:load (erlrc);

load_application (X) ->
  application:load (X).

%load_application (App) ->
%  try
%    AppsDir = get_apps_dir (),
%    Spec = load_resource_file (AppsDir, App),
%    case application:load (Spec) of
%      ok ->
%        ok;
%      { error, LoadReason } ->
%        throw ({ error, LoadReason })
%    end
%  catch
%    throw:Error -> { error, Error }
%  end.

get_apps_dir () ->
  Root = case os:getenv ("ERLRC_ROOT") of
    false ->
      { ok, Dir } = application:get_env (erlrc, root_dir),
      Dir;
    Value ->
      Value
  end,
  Root ++ "/applications".

%% @private
load_resource_file (AppsDir, App) when is_atom (App) ->
  ResourceBasename = atom_to_list (App) ++ ".app",

  % try override ERLRC_ROOT/applications/APPLICATION.app
  OverrideResourceFile = AppsDir ++ "/" ++ ResourceBasename,

  AppSpec =
    case code:where_is_file (ResourceBasename) of
      non_existing ->
        throw ({ appspec_file_not_found, ResourceBasename });
      Path ->
        case file:consult (Path) of
          { ok, [ SSpec = { application, App, SList } ] }
            when is_list (SList) ->
              SSpec;
          { ok, [ Other ] } ->
            throw ({ appspec_file_parse_error, Path, { bad_format, Other } });
          { error, Reason } ->
            throw ({ appspec_file_parse_error, Path, Reason })
        end
    end,

  OverrideSpec =
    case file:consult (OverrideResourceFile) of
      { ok, [ OSpec = { application, App, OList } ] } when is_list (OList) ->
        OSpec;
      { error, enoent } ->
        { application, App, [] };
      { ok, [ Other2 ] } ->
        throw ({ appspec_file_parse_error,
                 OverrideResourceFile,
                 { bad_format, Other2 } });
      { error, Reason2 } ->
        throw ({ appspec_file_parse_error, OverrideResourceFile, Reason2 })
    end,

  appspec_merge (OverrideSpec, AppSpec).


appspec_merge ({ application, App, Overrides }, { application, App, Source }) ->
  appspec_merge (App, Overrides, Source).

appspec_merge (App, Overrides, Source)
  when is_atom (App), is_list (Source), is_list (Overrides) ->
    S = lists:ukeysort (1, Source),
    O = lists:ukeysort (1, Overrides),
    { application, App, appspec_sub_merge (S, O, S) }.

appspec_sub_merge (_S, [], C) ->
  C;
appspec_sub_merge (S, [ { Key, { merge, OV } } | O ], C) when is_list (OV) ->
  case lists:keysearch (Key, 1, S) of
    { _, { Key, SV } } when is_list (SV) ->
      ML = lists:umerge (lists:usort (OV), lists:usort (SV)),
      appspec_sub_merge (S, O, keystore (Key, 1, C, { Key, ML }));
    _ ->
      appspec_sub_merge (S, O, keystore (Key, 1, C, { Key, OV }))
  end;
appspec_sub_merge (S, [ { Key, { keymerge, OV } } | O ], C) when is_list (OV) ->
  case lists:keysearch (Key, 1, S) of
    { _, { Key, SV } } when is_list (SV) ->
      ML = lists:ukeymerge (1, lists:ukeysort (1, OV), lists:ukeysort (1, SV)),
      appspec_sub_merge (S, O, keystore (Key, 1, C, { Key, ML }));
    _ ->
      appspec_sub_merge (S, O, keystore (Key, 1, C, { Key, OV }))
  end;
appspec_sub_merge (S, [ { Key, { override, OV } } | O ], C) ->
  appspec_sub_merge (S, O, keystore (Key, 1, C, { Key, OV }));
appspec_sub_merge (S, [ { Key, OV } | O ], C) ->
  appspec_sub_merge (S, O, keystore (Key, 1, C, { Key, OV }));
appspec_sub_merge (_, [ Other | _ ], _) ->
  erlang:throw ({ appspec_override_wrong_arity, Other }).


% lists:keystore from R12 for R11, TODO use lists:keystore when it's there
keystore (K, N, L, New) when is_integer (N), N > 0, is_tuple (New) ->
    keystore2 (K, N, L, New).

keystore2 (Key, N, [ H | T ], New) when element (N, H) == Key ->
    [ New | T ];
keystore2 (Key, N, [ H | T ], New) ->
    [ H | keystore2 (Key, N, T, New) ];
keystore2 (_Key, _N, [], New) ->
    [ New ].

-ifdef (TEST).
-include_lib ("eunit/include/eunit.hrl").

asm_test0 (Spec, Override, Expected, Line) ->
  { application, my_app, Merged } =
    try
      appspec_merge ({ application, my_app, Override },
                     { application, my_app, Spec })
    catch C:E ->
      erlang:error ({ asm_test_exception, [ { exception, { C, E } },
                    { stack, erlang:get_stacktrace () },
                    { source, Spec },
                    { override, Override },
                    { line, Line } ] })

    end,
  case lists:all (fun (X) -> lists:member (X, Merged) end, Expected) of
    true  -> ok;
    false -> erlang:error ({ asm_test_badmatch,
                           [ { merged, Merged },
                             { expected, Expected },
                             { line, Line } ] })
  end.

-define (asm_test (X, Y, Z), asm_test0 (X, Y, Z, ?LINE)).
-define (_asm_test (X, Y, Z), ?_test (?asm_test (X, Y, Z))).

appspec_merge_test_ () ->
  SourceList =
    [
      { description, "a test application" },
      { vsn, "0.6.9" },
      { modules, [ module_1, module_2, module_3 ] },
      { registered, [ name_1, name_2, name_3 ] },
      { applications, [ kernel, stdlib, application_1, application_2 ] },
      { mod, { startmodule, [] } },
      { env, [ { env_1, "env_1" },
               { env_2, 2 },
               { env_3, "three" } ] }
    ],
  [
    ?_asm_test (SourceList,
                [],
                [ { vsn, "0.6.9" }, { mod, { startmodule, [] } } ]),
    ?_asm_test (SourceList, [], SourceList),
    ?_assertError ({ asm_test_badmatch, _ },
                   ?asm_test (SourceList,
                              [],
                              [ { vsn, "0.6.9" }, { env, [] } ])),
    ?_asm_test (SourceList,
                [ { env, { keymerge, [ { env_1, "one" },
                        { env_2, "two" } ] } } ],
                [ { env, [ { env_1, "one" },
                      { env_2, "two" },
                      { env_3, "three" } ] } ]),
    ?_asm_test (SourceList,
                [ { env, { keymerge, [ { env_1, "one" }, { env_2, "two" } ] } },
                  { modules, { merge, [ module_4, module_5 ] } } ],
                [ { env, [ { env_1, "one" },
                      { env_2, "two" },
                      { env_3, "three" } ] },
                  { modules, [ module_1, module_2, module_3,
                      module_4, module_5 ] } ]),
    ?_asm_test (SourceList,
                [ { env, { keymerge, [ { env_1, "one" },
                        { env_2, "two" } ] } },
                  { modules, { override, [ module_4, module_5 ] } } ],
                [ { env, [ { env_1, "one" },
                      { env_2, "two" },
                      { env_3, "three" } ] },
                  { modules, [ module_4, module_5 ] } ]),
    ?_asm_test (SourceList,
                [ { env, { keymerge, [ { env_1, "one" },
                        { env_2, "two" } ] } },
                  { modules, [ module_4, module_5 ] } ],
                [ { env, [ { env_1, "one" },
                      { env_2, "two" },
                      { env_3, "three" } ] },
                  { modules, [ module_4, module_5 ] } ]),
    ?_asm_test (SourceList,
                [ { blah, { keymerge, [ { env_1, "one" },
                        { env_2, "two" } ] } },
                  { blodules, { merge, [ module_4, module_5 ] } } ],
                [ { blah, [ { env_1, "one" },
                      { env_2, "two" } ] },
                  { blodules, [ module_4, module_5 ] } ]),
    ?_assertError ({ asm_test_exception,
                     [ { exception,
                         { _, { appspec_override_wrong_arity, _ } } } | _ ] },
                   ?asm_test (SourceList,
                              [ { env, { keymerge, [ { env_1, "one" },
                                                     { env_2, "two" } ] } },
                                { odd, 'size', tuple } ],
                              [ { odd, 'size', tuple } ])),
    ?_test (ok)
  ].

-endif.
