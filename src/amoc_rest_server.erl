-module(amoc_rest_server).


-define(DEFAULT_LOGIC_HANDLER, amoc_rest_default_logic_handler).

-export([start/2]).

-spec start( ID :: any(), #{
    ip            => inet:ip_address(),
    port          => inet:port_number(),
    logic_handler => module(),
    net_opts      => []
}) -> {ok, pid()} | {error, any()}.

start(ID, #{
    ip            := IP ,
    port          := Port,
    net_opts      := NetOpts
} = Params) ->
    {Transport, TransportOpts} = get_socket_transport(IP, Port, NetOpts),
    LogicHandler = maps:get(logic_handler, Params, ?DEFAULT_LOGIC_HANDLER),
    CowboyOpts = get_default_opts(LogicHandler),
    case Transport of
        ssl ->
            cowboy:start_tls(ID, TransportOpts, CowboyOpts);
        tcp ->
            cowboy:start_clear(ID, TransportOpts, CowboyOpts)
    end.

get_socket_transport(IP, Port, Options) ->
    Opts = [
        {ip,   IP},
        {port, Port}
    ],
    case amoc_rest_utils:get_opt(ssl, Options) of
        SslOpts = [_|_] ->
            {ssl, Opts ++ SslOpts};
        undefined ->
            {tcp, Opts}
    end.

get_default_dispatch(LogicHandler) ->
    [{'_', DefaultPaths}] = amoc_rest_router:get_paths(LogicHandler),
    Paths = [{'_', [ %% adding static routing for swagger-ui
        {"/api-docs", cowboy_static, {priv_file, amoc_rest, "swagger_ui/index.html"}},
        {"/api-docs/[...]", cowboy_static, {priv_dir, amoc_rest, "swagger_ui"}},
        {"/openapi.json", cowboy_static, {priv_file, amoc_rest, "openapi.json"}} |
        DefaultPaths]}],
    #{dispatch => cowboy_router:compile(Paths)}.

get_default_opts(LogicHandler) ->
    #{env => get_default_dispatch(LogicHandler)}.
