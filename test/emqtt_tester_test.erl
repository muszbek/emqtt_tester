%% @author tmuszbek
%% @doc @todo Add description to emqtt_tester_test.


-module(emqtt_tester_test).
-include_lib("eunit/include/eunit.hrl").
-compile([{parse_transform, lager_transform}]).

-define(setup(F), {foreach, fun setup/0, fun cleanup/1, F}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTER DATA DEFINITIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(MQTT_ADDRESS, {"127.0.0.1", 1883, "guest", ""}).
-define(TEST_ASSERTIONS, [{}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%% fsm_starts_first_test_() ->
%% 	{"The fsm waits in down state until the python server is up at startup.",
%% 	 ?setup([fun fsm_before_server/1,
%% 			 fun server_after_fsm/1])}.
%% 
%% server_starts_first_test_() ->
%% 	{"The fsm starts up at idle state if the server is already up.",
%% 	 ?setup([fun fsm_after_server/1])}.


%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%

setup() ->
	meck:new(emqttc),
	meck:expect(emqttc, start_link, fun start_mock_emqttc/1),
	meck:expect(emqttc, subscribe, fun mock_subscribe/3),
	meck:expect(emqttc, unsubscribe, fun mock_unsubscribe/2),
	meck:expect(emqttc, publish, fun mock_publish/3),
	ok.

cleanup(_) ->
	meck:unload(emqttc),
	ok.

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%

start_mock_emqttc(_ArgList) ->
	Conn = mock_conn,
	{ok, Conn}.

mock_subscribe(_Conn, _Topic, _Qos) ->
	ok.

mock_unsubscribe(_Conn, _Topic) ->
	ok.
	
mock_publish(_Conn, Topic, Payload) ->
	self() ! {publish, Topic, Payload}.
