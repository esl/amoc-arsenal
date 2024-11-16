-module(amoc_api_logic_handler).

-behaviour(amoc_rest_logic_handler).

-include_lib("kernel/include/logger.hrl").

-export([set_validator_state/0, get_validator_state/0]).

-ifdef(TEST).
-export([set_validator_state/1]).
-endif.

-export([api_key_callback/2, accept_callback/4, provide_callback/4]).

-spec api_key_callback(amoc_rest_api:operation_id(), binary()) -> {true, #{}}.
api_key_callback(OperationID, ApiKey) ->
    ?LOG_WARNING(#{
        what => invalid_authentication_request,
        message => "AMOC REST API does not implement any authentication",
        operation_id => OperationID,
        api_key => ApiKey
    }),
    {true, #{}}.

-spec accept_callback(
    amoc_rest_api:class(),
    amoc_rest_api:operation_id(),
    cowboy_req:req(),
    amoc_rest_logic_handler:context()
) ->
    {
        amoc_rest_logic_handler:accept_callback_return(),
        cowboy_req:req(),
        amoc_rest_logic_handler:context()
    }.
accept_callback(scenarios, uploadNewScenario, Req0, Context0) ->
    {ok, ModuleSource, Req1} = read_entire_body(Req0),
    case amoc_api_helpers_scenario_upload:upload(ModuleSource) of
        {error, invalid_module} ->
            Body = json:encode(#{<<"error">> => <<"invalid module">>}),
            Req2 = cowboy_req:set_resp_body(Body, Req1),
            {false, Req2, Context0};
        {error, Error} ->
            Body = json:encode(#{<<"compile">> => Error}),
            Req2 = cowboy_req:set_resp_body(Body, Req1),
            {true, Req2, Context0};
        ok ->
            Body = json:encode(#{<<"compile">> => <<"ok">>}),
            Req2 = cowboy_req:set_resp_body(Body, Req1),
            {true, Req2, Context0}
    end;
accept_callback(Class, OperationID, Req0, Context0) ->
    VState = get_validator_state(),
    maybe
        {ok, Model, Req1} ?= amoc_rest_api:populate_request(OperationID, Req0, VState),
        Context1 = maps:merge(Context0, Model),
        {ok, Code, Response, Req2} ?= do_accept(Class, OperationID, Req1, Context1),
        ok ?= amoc_rest_api:validate_response(OperationID, Code, Response, VState),
        Req3 = cowboy_req:set_resp_body(json:encode(Response), Req2),
        {true, Req3, Context1}
    else
        Else ->
            process_error(Class, OperationID, Req0, Context0, accept, Else)
    end.

-spec provide_callback(
        amoc_rest_api:class(),
        amoc_rest_api:operation_id(),
        cowboy_req:req(),
        amoc_rest_logic_handler:context()) ->
    {
        amoc_rest_logic_handler:provide_callback_return(),
        cowboy_req:req(),
        amoc_rest_logic_handler:context()
    }.
provide_callback(Class, OperationID, Req0, Context0) ->
    VState = get_validator_state(),
    maybe
        {ok, Model, Req1} ?= amoc_rest_api:populate_request(OperationID, Req0, VState),
        Context1 = maps:merge(Context0, Model),
        {ok, Response} ?= do_provide(Class, OperationID, Req1, Context1),
        ok ?= amoc_rest_api:validate_response(OperationID, 200, Response, VState),
        {json:encode(Response), Req1, Context1}
    else
        Else ->
            process_error(Class, OperationID, Req0, Context0, accept, Else)
    end.

process_error(Class, OperationID, Req0, Context0, Type, {error, Code}) when is_integer(Code) ->
    ?LOG_WARNING(#{what => invalid_http_request,
                   type => Type,
                   class => Class,
                   operation_id => OperationID}),
    Req = cowboy_req:reply(Code, Req0),
    {stop, Req, Context0};
process_error(Class, OperationID, Req0, Context0, Type, {error, Reason}) ->
    ?LOG_WARNING(#{what => invalid_response,
                   type => Type,
                   class => Class,
                   operation_id => OperationID,
                   reason => Reason}),
    Req = cowboy_req:reply(400, Req0),
    {stop, Req, Context0};
process_error(Class, OperationID, _, Context0, Type, {error, Reason, Req}) ->
    ?LOG_WARNING(#{what => invalid_http_request,
                   type => Type,
                   class => Class,
                   operation_id => OperationID,
                   reason => Reason}),
    Req1 = cowboy_req:reply(400, Req),
    {stop, Req1, Context0};
process_error(Class, OperationID, _, Context0, Type, {error, Code, Response, Req}) ->
    ?LOG_WARNING(#{what => invalid_http_request,
                   type => Type,
                   class => Class,
                   operation_id => OperationID,
                   reason => Response}),
    Req1 = cowboy_req:reply(Code, #{}, json:encode(Response), Req),
    {stop, Req1, Context0}.

do_accept(execution, addUsers, Req, #{'ExecutionChangeUsers' := Body}) ->
    case amoc_dist:get_state() of
        running ->
            Ret = amoc_api_helpers_execution:add_users(Body),
            process_ret_value(Ret, Req);
        _ ->
            {error, 409, #{}, Req}
    end;
do_accept(execution, removeUsers, Req, #{'ExecutionChangeUsers' := Body}) ->
    case amoc_dist:get_state() of
        running ->
            Ret = amoc_api_helpers_execution:remove_users(Body),
            process_ret_value(Ret, Req);
        _ ->
            {error, 409, #{}, Req}
    end;
do_accept(execution, updateSettings, Req, #{'ExecutionUpdateSettings' := Body}) ->
    case amoc_dist:get_state() of
        running ->
            Ret = amoc_api_helpers_execution:update_settings(Body),
            process_ret_value(Ret, Req);
        _ ->
            {error, 409, #{}, Req}
    end;
do_accept(execution, startScenario, Req, #{'ExecutionStart' := Body}) ->
    case amoc_dist:get_state() of
        idle ->
            Ret = amoc_api_helpers_execution:start(Body),
            process_ret_value(Ret, Req);
        _ ->
            {error, 409, #{}, Req}
    end;
do_accept(execution, stopScenario, Req, _Context) ->
    case amoc_dist:get_state() of
        running ->
            Ret = amoc_api_helpers_execution:stop(),
            process_ret_value(Ret, Req);
        _ ->
            {error, 409, #{}, Req}
    end;
do_accept(Class, OperationID, Req, Context) ->
    ?LOG_ERROR(#{
        what => "Got not implemented request to process",
        class => Class,
        operation_id => OperationID,
        request => Req,
        context => Context
    }),
    {error, 404, #{}, Req}.

do_provide(scenarios, getAvailableScenarios, _Req, _Context) ->
    Scenarios = amoc_code_server:list_scenario_modules(),
    BinaryScenarios = [atom_to_binary(S, utf8) || S <- Scenarios],
    {ok, #{scenarios => BinaryScenarios}};
do_provide(scenarios, getScenarioDescription, _Req, #{id := ScenarioName}) ->
    case amoc_api_helpers_scenario_info:is_loaded(ScenarioName) of
        false ->
            {error, 404};
        {true, Scenario} ->
            EDoc = amoc_api_helpers_scenario_info:get_documentation(Scenario),
            Params = amoc_api_helpers_scenario_info:scenario_params(Scenario),
            {ok, #{doc => EDoc, parameters => Params}}
    end;
do_provide(scenarios, getScenarioSettings, _Req, #{id := ScenarioName}) ->
    case amoc_api_helpers_scenario_info:is_loaded(ScenarioName) of
        false ->
            {error, 404};
        {true, Scenario} ->
            Settings = amoc_api_helpers_scenario_info:scenario_settings(Scenario),
            {ok, #{settings => Settings}}
    end;
do_provide(status, getAmocAppStatus, _Req, _Context) ->
    {ok, amoc_api_helpers_status:get_status()};
do_provide(status, getAmocAppStatusOnNode, _Req, #{node := BinNode}) ->
    try
        Node = binary_to_existing_atom(BinNode),
        case erpc:call(Node, amoc_api_helpers_status, get_status, []) of
            {badrpc, _} -> {error, 404};
            Status -> {ok, Status}
        end
    catch
        _:_ ->
            {error, 404}
    end;
do_provide(status, getClusteredNodes, _Req1, _Context1) ->
    #{connected := Connected,
      failed_to_connect := FailedToConnect,
      connection_lost := ConnectionLost} = amoc_cluster:get_status(),
    Up = [{Node, <<"up">>} || Node <- [node() | Connected]],
    DownNodes = lists:usort(FailedToConnect ++ ConnectionLost),
    Down = [{Node, <<"down">>} || Node <- DownNodes],
    {ok, #{nodes => maps:from_list(Up ++ Down)}};
do_provide(Class, OperationID, Req, Context) ->
    ?LOG_ERROR(#{
        what => "Got not implemented request to process",
        class => Class,
        operation_id => OperationID,
        request => Req,
        context => Context
    }),
    {error, 404}.

read_entire_body(Request) ->
    read_entire_body(Request, <<>>).

read_entire_body(Request, Accumulator) ->
    try cowboy_req:read_body(Request) of
        {ok, Data, NewRequest} ->
            {ok, <<Accumulator/binary, Data/binary>>, NewRequest};
        {more, Data, NewRequest} ->
            read_entire_body(NewRequest, << Accumulator/binary, Data/binary >>)
    catch
        error:Error ->
            ?LOG_ERROR("Received error: ~p~n", [Error]),
            {error, <<>>, Request}
    end.

process_ret_value({ok, _}, Req) ->
    {ok, 200, #{}, Req};
process_ret_value({error, Error}, Req) ->
    {error, 500, #{error => amoc_config_parser:format(Error, binary)}, Req}.
-spec set_validator_state() -> ok.
set_validator_state() ->
    ValidatorState = amoc_rest_api:prepare_validator(),
    persistent_term:put(?MODULE, ValidatorState).

-ifdef(TEST).
-spec set_validator_state(_) -> ok.
set_validator_state(Path) ->
    ValidatorState = amoc_rest_api:prepare_validator(
        Path, <<"http://json-schema.org/draft-06/schema#">>
    ),
    persistent_term:put(?MODULE, ValidatorState).
-endif.

-spec get_validator_state() -> jesse_state:state().
get_validator_state() ->
    persistent_term:get(?MODULE).
