-module(amoc_rest_router).

-export([get_paths/1]).

-type method() :: binary().
-type operations() :: #{method() => amoc_rest_api:operation_id()}.
-type init_opts()  :: {operations(), module()}.

-export_type([init_opts/0]).

-spec get_paths(LogicHandler :: module()) -> cowboy_router:routes().
get_paths(LogicHandler) ->
    PreparedPaths = maps:fold(
                      fun(Path, #{operations := Operations, handler := Handler}, Acc) ->
                              [{Path, Handler, Operations} | Acc]
                      end, [], group_paths()
                     ),
    [{'_', [{P, H, {O, LogicHandler}} || {P, H, O} <- PreparedPaths]}].

group_paths() ->
    maps:fold(
      fun(OperationID, #{servers := Servers, base_path := BasePath, path := Path,
                         method := Method, handler := Handler}, Acc) ->
              FullPaths = build_full_paths(Servers, BasePath, Path),
              merge_paths(FullPaths, OperationID, Method, Handler, Acc)
      end, #{}, get_operations()).

build_full_paths([], BasePath, Path) ->
    [lists:append([BasePath, Path])];
build_full_paths(Servers, _BasePath, Path) ->
    [lists:append([Server, Path]) || Server <- Servers ].

merge_paths(FullPaths, OperationID, Method, Handler, Acc) ->
    lists:foldl(
      fun(Path, Acc0) ->
              case maps:find(Path, Acc0) of
                  {ok, PathInfo0 = #{operations := Operations0}} ->
                      Operations = Operations0#{Method => OperationID},
                      PathInfo = PathInfo0#{operations => Operations},
                      Acc0#{Path => PathInfo};
                  error ->
                      Operations = #{Method => OperationID},
                      PathInfo = #{handler => Handler, operations => Operations},
                      Acc0#{Path => PathInfo}
              end
      end, Acc, FullPaths).

get_operations() ->
    #{ 
       'addUsers' => #{
            servers => [],
            base_path => "",
            path => "/execution/add_users",
            method => <<"PATCH">>,
            handler => 'amoc_rest_execution_handler'
        },
       'removeUsers' => #{
            servers => [],
            base_path => "",
            path => "/execution/remove_users",
            method => <<"PATCH">>,
            handler => 'amoc_rest_execution_handler'
        },
       'startScenario' => #{
            servers => [],
            base_path => "",
            path => "/execution/start",
            method => <<"PATCH">>,
            handler => 'amoc_rest_execution_handler'
        },
       'stopScenario' => #{
            servers => [],
            base_path => "",
            path => "/execution/stop",
            method => <<"PATCH">>,
            handler => 'amoc_rest_execution_handler'
        },
       'updateSettings' => #{
            servers => [],
            base_path => "",
            path => "/execution/update_settings",
            method => <<"PATCH">>,
            handler => 'amoc_rest_execution_handler'
        },
       'getAvailableScenarios' => #{
            servers => [],
            base_path => "",
            path => "/scenarios",
            method => <<"GET">>,
            handler => 'amoc_rest_scenarios_handler'
        },
       'getScenarioDescription' => #{
            servers => [],
            base_path => "",
            path => "/scenarios/info/:id",
            method => <<"GET">>,
            handler => 'amoc_rest_scenarios_handler'
        },
       'getScenarioSettings' => #{
            servers => [],
            base_path => "",
            path => "/scenarios/defaults/:id",
            method => <<"GET">>,
            handler => 'amoc_rest_scenarios_handler'
        },
       'uploadNewScenario' => #{
            servers => [],
            base_path => "",
            path => "/scenarios/upload",
            method => <<"PUT">>,
            handler => 'amoc_rest_scenarios_handler'
        },
       'getAmocAppStatus' => #{
            servers => [],
            base_path => "",
            path => "/status",
            method => <<"GET">>,
            handler => 'amoc_rest_status_handler'
        },
       'getAmocAppStatusOnNode' => #{
            servers => [],
            base_path => "",
            path => "/status/:node",
            method => <<"GET">>,
            handler => 'amoc_rest_status_handler'
        },
       'getClusteredNodes' => #{
            servers => [],
            base_path => "",
            path => "/nodes",
            method => <<"GET">>,
            handler => 'amoc_rest_status_handler'
        }
    }.
