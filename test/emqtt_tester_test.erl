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
-define(TEST_ASSERTIONS, [{<<"topic_first">>, <<"payload_first">>, "Message_first!"},
						  {<<"topic_second">>, <<"payload_second">>, "Message_second!"}]).

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
	Subscriptions = ets:new({mock_conn, [duplicate_bag]}),
	Conn = {self(), Subscriptions},
	self() ! {mqttc, Conn, connected},
	{ok, Conn}.

mock_subscribe(Conn, Topic, _Qos) ->
	{_TestProc, Subscriptions} = Conn,
	true = ets:insert(Subscriptions, Topic),
	ok.

mock_unsubscribe(Conn, Topic) ->
	{_TestProc, Subscriptions} = Conn,
	true = ets:delete(Subscriptions, Topic),
	ok.
	
mock_publish(Conn, Topic, Payload) ->
	{TestProc, Subscriptions} = Conn,
	
	case ets:lookup(Conn, Topic) of
		[] -> 
			lager:debug("Test publishing to an unsubscribed topic ~p, ignored", [Topic]),
			ok;
		[_|_] ->	%% lookup returns not empty list, Topic is present in ets
			lager:debug("Test publishing to subscrubed topic ~p", [Topic]),
			TestProc ! {publish, Topic, Payload},
			ok
	end.


action_to_test_only_asserted(Conn) ->
	emqttc:publish(Conn, <<"topic_first">>, <<"payload_first">>),
	emqttc:publish(Conn, <<"topic_second">>, <<"payload_second">>).
	
