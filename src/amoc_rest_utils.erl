-module(amoc_rest_utils).

-export([to_binary/1]).
-export([to_list/1]).
-export([to_float/1]).
-export([to_int/1]).
-export([to_lower/1]).
-export([to_upper/1]).
-export([set_resp_headers/2]).
-export([to_header/1]).
-export([to_qs/1]).
-export([to_binding/1]).
-export([get_opt/2]).
-export([get_opt/3]).
-export([priv_dir/0]).
-export([priv_dir/1]).
-export([priv_path/1]).


-spec to_binary(iodata() | atom() | number()) -> binary().

to_binary(V) when is_binary(V)  -> V;
to_binary(V) when is_list(V)    -> iolist_to_binary(V);
to_binary(V) when is_atom(V)    -> atom_to_binary(V, utf8);
to_binary(V) when is_integer(V) -> integer_to_binary(V);
to_binary(V) when is_float(V)   -> float_to_binary(V).

-spec to_list(iodata() | atom() | number()) -> string().

to_list(V) when is_list(V)      -> V;
to_list(V)                      -> binary_to_list(to_binary(V)).

-spec to_float(iodata()) -> number().

to_float(V) ->
    Data = iolist_to_binary([V]),
    case binary:split(Data, <<$.>>) of
        [Data] ->
            binary_to_integer(Data);
        [<<>>, _] ->
            binary_to_float(<<$0, Data/binary>>);
        _ ->
            binary_to_float(Data)
    end.

%%

-spec to_int(integer() | binary() | list()) -> integer().

to_int(Data) when is_integer(Data) ->
    Data;
to_int(Data) when is_binary(Data) ->
    binary_to_integer(Data);
to_int(Data) when is_list(Data) ->
    list_to_integer(Data).

-spec set_resp_headers([{binary(), iodata()}], cowboy_req:req()) -> cowboy_req:req().

set_resp_headers([], Req) ->
    Req;
set_resp_headers([{K, V} | T], Req0) ->
    Req = cowboy_req:set_resp_header(K, V, Req0),
    set_resp_headers(T, Req).

-spec to_header(iodata() | atom() | number()) -> binary().

to_header(Name) ->
    Prepared = to_binary(Name),
    to_lower(Prepared).

-spec to_qs(iodata() | atom() | number()) -> binary().

to_qs(Name) ->
    to_binary(Name).

-spec to_binding(iodata() | atom() | number()) -> atom().

to_binding(Name) ->
    Prepared = to_binary(Name),
    binary_to_atom(Prepared, utf8).

-spec get_opt(any(), []) -> any().

get_opt(Key, Opts) ->
    get_opt(Key, Opts, undefined).

-spec get_opt(any(), [], any()) -> any().

get_opt(Key, Opts, Default) ->
    case lists:keyfind(Key, 1, Opts) of
        {_, Value} -> Value;
        false -> Default
    end.

-spec priv_dir() -> file:filename().

priv_dir() ->
    {ok, AppName} = application:get_application(),
    priv_dir(AppName).

-spec priv_dir(Application :: atom()) -> file:filename().

priv_dir(AppName) ->
    case code:priv_dir(AppName) of
        Value when is_list(Value) ->
            Value ++ "/";
        _Error ->
            select_priv_dir([filename:join(["apps", atom_to_list(AppName), "priv"]), "priv"])
     end.

-spec priv_path(Relative :: file:filename()) -> file:filename().

priv_path(Relative) ->
    filename:join(priv_dir(), Relative).

-include_lib("kernel/include/file.hrl").

select_priv_dir(Paths) ->
    case lists:dropwhile(fun test_priv_dir/1, Paths) of
        [Path | _] -> Path;
        _          -> exit(no_priv_dir)
    end.

test_priv_dir(Path) ->
    case file:read_file_info(Path) of
        {ok, #file_info{type = directory}} ->
            false;
        _ ->
            true
    end.


%%

-spec to_lower(binary()) -> binary().

to_lower(S) ->
    to_case(lower, S, <<>>).

-spec to_upper(binary()) -> binary().

to_upper(S) ->
    to_case(upper, S, <<>>).

to_case(_Case, <<>>, Acc) ->
    Acc;

to_case(_Case, <<C, _/binary>>, _Acc) when C > 127 ->
    error(badarg);

to_case(Case = lower, <<C, Rest/binary>>, Acc) ->
    to_case(Case, Rest, <<Acc/binary, (to_lower_char(C))>>);

to_case(Case = upper, <<C, Rest/binary>>, Acc) ->
    to_case(Case, Rest, <<Acc/binary, (to_upper_char(C))>>).

to_lower_char(C) when is_integer(C), $A =< C, C =< $Z ->
    C + 32;
to_lower_char(C) when is_integer(C), 16#C0 =< C, C =< 16#D6 ->
    C + 32;
to_lower_char(C) when is_integer(C), 16#D8 =< C, C =< 16#DE ->
    C + 32;
to_lower_char(C) ->
    C.

to_upper_char(C) when is_integer(C), $a =< C, C =< $z ->
    C - 32;
to_upper_char(C) when is_integer(C), 16#E0 =< C, C =< 16#F6 ->
    C - 32;
to_upper_char(C) when is_integer(C), 16#F8 =< C, C =< 16#FE ->
    C - 32;
to_upper_char(C) ->
    C.
