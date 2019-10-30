%% @author tmuszbek
%% @doc @todo Add description to emqtt_tester_test.


-module(emqtt_tester_test).
-include_lib("eunit/include/eunit.hrl").
-compile([{parse_transform, lager_transform}]).

-define(setup(FunList), {foreach, fun setup/0, fun cleanup/1, FunList}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTER DATA DEFINITIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(MQTT_ADDRESS, {"127.0.0.1", 1883, "guest", ""}).
-define(TEST_ASSERTIONS, [{<<"topic_first">>, <<"payload_first">>, "Message_first!", 15},
						  {<<"topic_second">>, <<"payload_second">>, "Message_second!", 15}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

assertions_succeed_test_() ->
	{"The test run succeeds all assertions when the messages arrive in correct order.",
	 ?setup([fun publish_only_asserted_succeeds/1,
			 fun publish_unsubscribed_topic_succeeds/1,
			 fun publish_wrong_payload_succeeds/1,
			 fun publish_duplicate_payload_succeeds/1,
			 fun send_unrelated_message_succeeds/1,
			 fun action_to_test_down_before_assert_succeeds/1])}.

on_error_fail_test_() ->
	{"Any processes crashing during the tests return an error.",
	 ?setup([fun emqtt_disconnect_returns_error/1,
			 fun action_to_test_crashes_returns_error/1,
			 fun action_to_test_exits_returns_error/1])}.

assertions_fail_on_timeout_test_() ->
	{"The test run fails all assertions which do not receive their message before timeout.",
	 ?setup([fun publish_asserted_with_delay_fails/1,
			 fun publish_asserted_succeeds_after_failure/1])}.


%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%

setup() ->
	meck:new(emqttc),
	meck:expect(emqttc, start_link, fun start_mock_emqttc/1),
	meck:expect(emqttc, subscribe, fun mock_subscribe/3),
	meck:expect(emqttc, unsubscribe, fun mock_unsubscribe/2),
	meck:expect(emqttc, publish, fun mock_publish/3),
	meck:expect(emqttc, disconnect, fun mock_disconnect/1),
	ok.

cleanup(_) ->
	meck:unload(emqttc),
	ok.

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%

%% assertions_succeed_test

publish_only_asserted_succeeds(_) ->
	assert_success(fun action_to_test_only_asserted/1).

publish_unsubscribed_topic_succeeds(_) ->
	assert_success(fun action_to_test_unsubscribed_topic/1).

publish_wrong_payload_succeeds(_) ->
	assert_success(fun action_to_test_wrong_payload/1).

publish_duplicate_payload_succeeds(_) ->
	assert_success(fun action_to_test_duplicate_payload/1).

send_unrelated_message_succeeds(_) ->
	assert_success(fun action_to_test_unrelated_message/1).

action_to_test_down_before_assert_succeeds(_) ->
	assert_success(fun action_to_test_normal_down_message/1).


%% on_emqtt_disconnect_fail_test

emqtt_disconnect_returns_error(_) ->
	Results = emqtt_tester:run(?MQTT_ADDRESS, ?TEST_ASSERTIONS,
							   fun action_to_test_emqtt_disconnects/1),
	?_assertEqual(Results, {error, mqttc_disconnected}).

action_to_test_crashes_returns_error(_) ->
	Results = emqtt_tester:run(?MQTT_ADDRESS, ?TEST_ASSERTIONS,
							   fun action_to_test_crashes/1),
	?_assertEqual(Results, {error, test_action_crashed}).

action_to_test_exits_returns_error(_) ->
	Results = emqtt_tester:run(?MQTT_ADDRESS, ?TEST_ASSERTIONS,
							   fun action_to_test_exits/1),
	?_assertEqual(Results, {error, test_action_crashed}).


%% assertions_fail_on_timeout_test

publish_asserted_with_delay_fails(_) ->
	Results = emqtt_tester:run(?MQTT_ADDRESS, ?TEST_ASSERTIONS,
							   fun action_to_test_assertion_timeout/1),
	?_assertEqual(Results, [success, failure]).

publish_asserted_succeeds_after_failure(_) ->
	Results = emqtt_tester:run(?MQTT_ADDRESS, ?TEST_ASSERTIONS,
							   fun action_to_test_assertion_timeout_then_succeed/1),
	?_assertEqual(Results, [failure, success]).


%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%

start_mock_emqttc(_ArgList) ->
	Subscriptions = ets:new(mock_conn, [duplicate_bag]),
	Conn = {self(), Subscriptions},
	self() ! {mqttc, Conn, connected},
	{ok, Conn}.

mock_subscribe(Conn, Topic, _Qos) ->
	{_TestProc, Subscriptions} = Conn,
	true = ets:insert(Subscriptions, {Topic}),
	ok.

mock_unsubscribe(Conn, Topic) ->
	{_TestProc, Subscriptions} = Conn,
	true = ets:delete(Subscriptions, Topic),
	ok.
	
mock_publish(Conn, Topic, Payload) ->
	{TestProc, Subscriptions} = Conn,
	
	case ets:lookup(Subscriptions, Topic) of
		[] -> 
			lager:debug("Test publishing to an unsubscribed topic ~p, ignored", [Topic]),
			ok;
		[_|_] ->	%% lookup returns not empty list, Topic is present in ets
			lager:debug("Test publishing to subscrubed topic ~p", [Topic]),
			TestProc ! {publish, Topic, Payload},
			ok
	end.

mock_disconnect(Conn) ->
	{_TestProc, Subscriptions} = Conn,
	true = ets:delete(Subscriptions),
	ok.


assert_success(ActionToTest) ->
	Results = emqtt_tester:run(?MQTT_ADDRESS, ?TEST_ASSERTIONS, ActionToTest),
	?_assertEqual(Results, [success, success]).


%% functions to pass as ActionToTest
action_to_test_only_asserted(Conn) ->
	emqttc:publish(Conn, <<"topic_first">>, <<"payload_first">>),
	emqttc:publish(Conn, <<"topic_second">>, <<"payload_second">>).

action_to_test_unsubscribed_topic(Conn) ->
	{TestProc, _Subs} = Conn,	%% in this mock context
	emqttc:publish(Conn, <<"topic_first">>, <<"payload_first">>),
	TestProc ! {publish, <<"topic_unsubscribed">>, <<"payload_second">>},
	emqttc:publish(Conn, <<"topic_second">>, <<"payload_second">>).

action_to_test_wrong_payload(Conn) ->
	emqttc:publish(Conn, <<"topic_first">>, <<"payload_first">>),
	emqttc:publish(Conn, <<"topic_second">>, <<"payload_first">>),
	emqttc:publish(Conn, <<"topic_second">>, <<"payload_second">>).

action_to_test_duplicate_payload(Conn) ->
	emqttc:publish(Conn, <<"topic_first">>, <<"payload_first">>),
	emqttc:publish(Conn, <<"topic_first">>, <<"payload_first">>),
	emqttc:publish(Conn, <<"topic_second">>, <<"payload_second">>).

action_to_test_unrelated_message(Conn) ->
	{TestProc, _Subs} = Conn,	%% in this mock context
	emqttc:publish(Conn, <<"topic_first">>, <<"payload_first">>),
	TestProc ! completely_different_message,
	emqttc:publish(Conn, <<"topic_second">>, <<"payload_second">>).

action_to_test_normal_down_message(Conn) ->
	emqttc:publish(Conn, <<"topic_first">>, <<"payload_first">>),
	spawn(fun() -> timer:sleep(10),
				   emqttc:publish(Conn, <<"topic_second">>, <<"payload_second">>)
		  end).


action_to_test_emqtt_disconnects(Conn) ->
	{TestProc, _Subs} = Conn,	%% in this mock context
	emqttc:publish(Conn, <<"topic_first">>, <<"payload_first">>),
	TestProc ! {mqttc, Conn, disconnected},	%% should throw error here
	emqttc:publish(Conn, <<"topic_second">>, <<"payload_second">>).

action_to_test_crashes(Conn) ->
	emqttc:publish(Conn, <<"topic_first">>, <<"payload_first">>),
	erlang:error(testing_error_case),
	emqttc:publish(Conn, <<"topic_second">>, <<"payload_second">>).

action_to_test_exits(Conn) ->
	emqttc:publish(Conn, <<"topic_first">>, <<"payload_first">>),
	exit(testing_exit_case),
	emqttc:publish(Conn, <<"topic_second">>, <<"payload_second">>).


action_to_test_assertion_timeout(Conn) ->
	emqttc:publish(Conn, <<"topic_first">>, <<"payload_first">>),
	timer:sleep(20),
	emqttc:publish(Conn, <<"topic_second">>, <<"payload_second">>).

action_to_test_assertion_timeout_then_succeed(Conn) ->
	timer:sleep(20),
	emqttc:publish(Conn, <<"topic_first">>, <<"payload_first">>),
	emqttc:publish(Conn, <<"topic_second">>, <<"payload_second">>).
	
