-module(amoc_rest_router).

-export([get_paths/1]).

-type operations() :: #{
    Method :: binary() => amoc_rest_api:operation_id()
}.

-type init_opts()  :: {
    Operations :: operations(),
    LogicHandler :: atom(),
    ValidatorState :: jesse_state:state()
}.

-export_type([init_opts/0]).

-spec get_paths(LogicHandler :: atom()) ->  [{'_',[{
    Path :: string(),
    Handler :: atom(),
    InitOpts :: init_opts()
}]}].

get_paths(LogicHandler) ->
    ValidatorState = prepare_validator(),
    PreparedPaths = maps:fold(
        fun(Path, #{operations := Operations, handler := Handler}, Acc) ->
            [{Path, Handler, Operations} | Acc]
        end,
        [],
        group_paths()
    ),
    [
        {'_',
            [{P, H, {O, LogicHandler, ValidatorState}} || {P, H, O} <- PreparedPaths]
        }
    ].

group_paths() ->
    maps:fold(
        fun(OperationID, #{path := Path, method := Method, handler := Handler}, Acc) ->
            case maps:find(Path, Acc) of
                {ok, PathInfo0 = #{operations := Operations0}} ->
                    Operations = Operations0#{Method => OperationID},
                    PathInfo = PathInfo0#{operations => Operations},
                    Acc#{Path => PathInfo};
                error ->
                    Operations = #{Method => OperationID},
                    PathInfo = #{handler => Handler, operations => Operations},
                    Acc#{Path => PathInfo}
            end
        end,
        #{},
        get_operations()
    ).

get_operations() ->
    #{ 
        'ExecutionAddUsersPatch' => #{
            path => "/execution/add_users",
            method => <<"PATCH">>,
            handler => 'amoc_rest_execution_handler'
        },
        'ExecutionRemoveUsersPatch' => #{
            path => "/execution/remove_users",
            method => <<"PATCH">>,
            handler => 'amoc_rest_execution_handler'
        },
        'ExecutionStartPatch' => #{
            path => "/execution/start",
            method => <<"PATCH">>,
            handler => 'amoc_rest_execution_handler'
        },
        'ExecutionStopPatch' => #{
            path => "/execution/stop",
            method => <<"PATCH">>,
            handler => 'amoc_rest_execution_handler'
        },
        'ExecutionUpdateSettingsPatch' => #{
            path => "/execution/update_settings",
            method => <<"PATCH">>,
            handler => 'amoc_rest_execution_handler'
        },
        'ScenariosDefaultsIdGet' => #{
            path => "/scenarios/defaults/:id",
            method => <<"GET">>,
            handler => 'amoc_rest_scenarios_handler'
        },
        'ScenariosGet' => #{
            path => "/scenarios",
            method => <<"GET">>,
            handler => 'amoc_rest_scenarios_handler'
        },
        'ScenariosInfoIdGet' => #{
            path => "/scenarios/info/:id",
            method => <<"GET">>,
            handler => 'amoc_rest_scenarios_handler'
        },
        'ScenariosUploadPut' => #{
            path => "/scenarios/upload",
            method => <<"PUT">>,
            handler => 'amoc_rest_scenarios_handler'
        },
        'NodesGet' => #{
            path => "/nodes",
            method => <<"GET">>,
            handler => 'amoc_rest_status_handler'
        },
        'StatusGet' => #{
            path => "/status",
            method => <<"GET">>,
            handler => 'amoc_rest_status_handler'
        },
        'StatusNodeGet' => #{
            path => "/status/:node",
            method => <<"GET">>,
            handler => 'amoc_rest_status_handler'
        }
    }.

prepare_validator() ->
    R = jsx:decode(element(2, file:read_file(get_openapi_path()))),
    jesse_state:new(R, [{default_schema_ver, <<"http://json-schema.org/draft-04/schema#">>}]).


get_openapi_path() ->
    {ok, AppName} = application:get_application(?MODULE),
    filename:join(amoc_rest_utils:priv_dir(AppName), "openapi.json").


