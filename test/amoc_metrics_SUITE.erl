-module(amoc_metrics_SUITE).

-behaviour(ct_suite).
-behaviour(amoc_scenario).

-compile([export_all, nowarn_export_all]).

-include_lib("stdlib/include/assert.hrl").

%% ct_suite callbacks

-spec all() -> [ct_suite:ct_test_def()].
all() ->
    [amoc_users_size_is_initialized,
     amoc_users_size_is_updated].

-spec init_per_testcase(ct_suite:ct_testname(), ct_suite:ct_config()) -> ct_suite:ct_config().
init_per_testcase(_TestCase, Config) ->
    amoc_api_helper:start_amoc(),
    Config.

-spec end_per_testcase(ct_suite:ct_testname(), ct_suite:ct_config()) -> ok.
end_per_testcase(_TestCase, _Config) ->
    amoc_api_helper:stop_amoc().

%% tests

-spec amoc_users_size_is_initialized(ct_suite:ct_config()) -> ok.
amoc_users_size_is_initialized(_Config) ->
    ?assertEqual(0, get_users_size()).

-spec amoc_users_size_is_updated(ct_suite:ct_config()) -> ok.
amoc_users_size_is_updated(_Config) ->
    amoc:do(?MODULE, 10, []),
    wait_for_users_size(10, 20),
    amoc:reset(),
    ?assertEqual(0, get_users_size()).

%% helpers

-spec wait_for_users_size(non_neg_integer(), non_neg_integer()) -> ok | no_return().
wait_for_users_size(ExpectedSize, Times) ->
    case get_users_size() of
        ExpectedSize ->
            ok;
        Size when Size < ExpectedSize, Times > 0 ->
            timer:sleep(100),
            wait_for_users_size(ExpectedSize, Times - 1);
        UnexpectedSize ->
            ct:fail("Unexpected amoc_users_size: ~p", [UnexpectedSize])
    end.

-spec get_users_size() -> non_neg_integer().
get_users_size() ->
    {ok, {Status, _Headers, Body}} = httpc:request("http://localhost:9090/metrics"),
    ?assertEqual({"HTTP/1.1", 200, "OK"}, Status),
    Pattern = "\namoc_users_size ([0-9]+)\n",
    {match, [SizeString]} = re:run(Body, Pattern, [{capture, all_but_first, list}]),
    list_to_integer(SizeString).

%% amoc_scenario callbacks

-spec init() -> ok.
init() ->
    ok.

-spec start(amoc_scenario:user_id()) -> ok.
start(_Id) ->
    timer:sleep(timer:seconds(10)).
