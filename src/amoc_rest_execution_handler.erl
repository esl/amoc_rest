-module(amoc_rest_execution_handler).
-moduledoc """
Exposes the following operation IDs:

- `PATCH` to `/execution/add_users`, OperationId: `addUsers`:
add new users.


- `PATCH` to `/execution/remove_users`, OperationId: `removeUsers`:
stop users.


- `PATCH` to `/execution/start`, OperationId: `startScenario`:
Start scenario.


- `PATCH` to `/execution/stop`, OperationId: `stopScenario`:
Stop scenario.


- `PATCH` to `/execution/update_settings`, OperationId: `updateSettings`:
update scenario settings.


""".

-behaviour(cowboy_rest).

-include_lib("kernel/include/logger.hrl").

%% Cowboy REST callbacks
-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).
-export([content_types_provided/2]).
-export([delete_resource/2]).
-export([is_authorized/2]).
-export([valid_content_headers/2]).
-export([handle_type_accepted/2, handle_type_provided/2]).

-ignore_xref([handle_type_accepted/2, handle_type_provided/2]).

-export_type([class/0, operation_id/0]).

-type class() :: 'execution'.

-type operation_id() ::
    'addUsers' %% add new users
    | 'removeUsers' %% stop users
    | 'startScenario' %% Start scenario
    | 'stopScenario' %% Stop scenario
    | 'updateSettings'. %% update scenario settings


-record(state,
        {operation_id :: operation_id(),
         accept_callback :: amoc_rest_logic_handler:accept_callback(),
         provide_callback :: amoc_rest_logic_handler:provide_callback(),
         api_key_handler :: amoc_rest_logic_handler:api_key_callback(),
         context = #{} :: amoc_rest_logic_handler:context()}).

-type state() :: #state{}.

-spec init(cowboy_req:req(), amoc_rest_router:init_opts()) ->
    {cowboy_rest, cowboy_req:req(), state()}.
init(Req, {Operations, Module}) ->
    Method = cowboy_req:method(Req),
    OperationID = maps:get(Method, Operations, undefined),
    ?LOG_INFO(#{what => "Attempt to process operation",
                method => Method,
                operation_id => OperationID}),
    State = #state{operation_id = OperationID,
                   accept_callback = fun Module:accept_callback/4,
                   provide_callback = fun Module:provide_callback/4,
                   api_key_handler = fun Module:authorize_api_key/2},
    {cowboy_rest, Req, State}.

-spec allowed_methods(cowboy_req:req(), state()) ->
    {[binary()], cowboy_req:req(), state()}.
allowed_methods(Req, #state{operation_id = 'addUsers'} = State) ->
    {[<<"PATCH">>], Req, State};
allowed_methods(Req, #state{operation_id = 'removeUsers'} = State) ->
    {[<<"PATCH">>], Req, State};
allowed_methods(Req, #state{operation_id = 'startScenario'} = State) ->
    {[<<"PATCH">>], Req, State};
allowed_methods(Req, #state{operation_id = 'stopScenario'} = State) ->
    {[<<"PATCH">>], Req, State};
allowed_methods(Req, #state{operation_id = 'updateSettings'} = State) ->
    {[<<"PATCH">>], Req, State};
allowed_methods(Req, State) ->
    {[], Req, State}.

-spec is_authorized(cowboy_req:req(), state()) ->
    {true | {false, iodata()}, cowboy_req:req(), state()}.
is_authorized(Req, State) ->
    {true, Req, State}.

-spec content_types_accepted(cowboy_req:req(), state()) ->
    {[{binary(), atom()}], cowboy_req:req(), state()}.
content_types_accepted(Req, #state{operation_id = 'addUsers'} = State) ->
    {[
      {<<"application/json">>, handle_type_accepted}
     ], Req, State};
content_types_accepted(Req, #state{operation_id = 'removeUsers'} = State) ->
    {[
      {<<"application/json">>, handle_type_accepted}
     ], Req, State};
content_types_accepted(Req, #state{operation_id = 'startScenario'} = State) ->
    {[
      {<<"application/json">>, handle_type_accepted}
     ], Req, State};
content_types_accepted(Req, #state{operation_id = 'stopScenario'} = State) ->
    {[
      {<<"application/json">>, handle_type_accepted}
     ], Req, State};
content_types_accepted(Req, #state{operation_id = 'updateSettings'} = State) ->
    {[
      {<<"application/json">>, handle_type_accepted}
     ], Req, State};
content_types_accepted(Req, State) ->
    {[], Req, State}.

-spec valid_content_headers(cowboy_req:req(), state()) ->
    {boolean(), cowboy_req:req(), state()}.
valid_content_headers(Req, #state{operation_id = 'addUsers'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'removeUsers'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'startScenario'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'stopScenario'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'updateSettings'} = State) ->
    {true, Req, State};
valid_content_headers(Req, State) ->
    {false, Req, State}.

-spec content_types_provided(cowboy_req:req(), state()) ->
    {[{binary(), atom()}], cowboy_req:req(), state()}.
content_types_provided(Req, #state{operation_id = 'addUsers'} = State) ->
    {[
      {<<"application/json">>, handle_type_provided}
     ], Req, State};
content_types_provided(Req, #state{operation_id = 'removeUsers'} = State) ->
    {[
      {<<"application/json">>, handle_type_provided}
     ], Req, State};
content_types_provided(Req, #state{operation_id = 'startScenario'} = State) ->
    {[
      {<<"application/json">>, handle_type_provided}
     ], Req, State};
content_types_provided(Req, #state{operation_id = 'stopScenario'} = State) ->
    {[
      {<<"application/json">>, handle_type_provided}
     ], Req, State};
content_types_provided(Req, #state{operation_id = 'updateSettings'} = State) ->
    {[
      {<<"application/json">>, handle_type_provided}
     ], Req, State};
content_types_provided(Req, State) ->
    {[], Req, State}.

-spec delete_resource(cowboy_req:req(), state()) ->
    {boolean(), cowboy_req:req(), state()}.
delete_resource(Req, State) ->
    {Res, Req1, State1} = handle_type_accepted(Req, State),
    {true =:= Res, Req1, State1}.

-spec handle_type_accepted(cowboy_req:req(), state()) ->
    { amoc_rest_logic_handler:accept_callback_return(), cowboy_req:req(), state()}.
handle_type_accepted(Req, #state{operation_id = OperationID,
                                 accept_callback = Handler,
                                 context = Context} = State) ->
    {Res, Req1, Context1} = Handler(execution, OperationID, Req, Context),
    {Res, Req1, State#state{context = Context1}}.

-spec handle_type_provided(cowboy_req:req(), state()) ->
    { amoc_rest_logic_handler:provide_callback_return(), cowboy_req:req(), state()}.
handle_type_provided(Req, #state{operation_id = OperationID,
                                 provide_callback = Handler,
                                 context = Context} = State) ->
    {Res, Req1, Context1} = Handler(execution, OperationID, Req, Context),
    {Res, Req1, State#state{context = Context1}}.
