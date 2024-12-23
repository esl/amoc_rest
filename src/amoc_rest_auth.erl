-module(amoc_rest_auth).

-export([authorize_api_key/5]).

-spec authorize_api_key(amoc_rest_logic_handler:api_key_callback(),
                        amoc_rest_api:operation_id(),
                        header | qs_val,
                        iodata() | atom(),
                        cowboy_req:req()) ->
    {true, amoc_rest_logic_handler:context(), cowboy_req:req()} |
    {false, binary(), cowboy_req:req()}.
authorize_api_key(Handler, OperationID, From, KeyParam, Req0) ->
    {ApiKey, Req} = get_api_key(From, KeyParam, Req0),
    case ApiKey of
        undefined ->
            AuthHeader = <<>>,
            {false, AuthHeader, Req};
        _ ->
            case Handler(OperationID, ApiKey) of
                {true, Context} ->
                    {true, Context, Req};
                {false, AuthHeader} ->
                    {false, AuthHeader, Req}
            end
    end.

get_api_key(header, KeyParam, Req) ->
    Headers = cowboy_req:headers(Req),
    {maps:get(KeyParam, Headers, undefined), Req};
get_api_key(qs_val, KeyParam, Req) ->
    QS = cowboy_req:parse_qs(Req),
    {get_opt(KeyParam, QS), Req}.

-spec get_opt(any(), []) -> any().
get_opt(Key, Opts) ->
    get_opt(Key, Opts, undefined).

-spec get_opt(any(), [], any()) -> any().
get_opt(Key, Opts, Default) ->
    case lists:keyfind(Key, 1, Opts) of
        {_, Value} ->
            Value;
        false ->
            Default
    end.
