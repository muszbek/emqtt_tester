-module(emqtt_tester).
-compile([{parse_transform, lager_transform}]).

-define(DEFAULT_TIMEOUT, 1000).

%% ====================================================================
%% API functions
%% ====================================================================
-export([run/3]).

%% MqttAddress: {Host, Port, User, Pw}
%% Assertions: [{Topic :: bin(), ExpectedPayload :: bin(), 
%%				 AssertionMessage :: string(), TimeOut :: non_neg_integer()}]
%% ActionToTest: fun((Conn) -> ok/error)
run(MqttAddress, Assertions, ActionToTest) ->
	Conn = emqttc_utils:connect_mqttc(MqttAddress),
	wait_until_connected(Conn, Assertions),
	
	TestAction = spawn_monitor(fun() -> ActionToTest(Conn) end),
	lager:debug("Test process spawned at ~p", [TestAction]),
	
	assert_message(Conn, Assertions, []).


%% ====================================================================
%% Internal functions
%% ====================================================================

wait_until_connected(Conn, Assertions) ->
	receive
		{mqttc, Conn, connected} ->
			lager:debug("Emqttc connected: ~p", [Conn]),
			emqttc_utils:subscribe_to_assertions(Conn, Assertions);
		
		Message ->
			lager:warning("Unexpected message: ~p~n", [Message]),
			wait_until_connected(Conn, Assertions)
		%% emqttc doesn't give any messages on timeout by default
	end.


assert_message(_Conn, [], Reports) ->
	report(Reports);

assert_message(Conn, Assertions, Reports) ->
	lager:debug("Calling assert, remaining assertions: ~p~n", [Assertions]),
	[CurrentAssertion | RemainingAssertions] = Assertions,
	{Topic, ExpectedPayload, AssertionMessage, TimeOut} = get_assertion(CurrentAssertion),
	
	receive
		{publish, Topic, ExpectedPayload} ->
			lager:debug("Assertion succeeded on topic ~p with payload ~p", 
						[Topic, ExpectedPayload]),
			NewReports = save_report(AssertionMessage, Reports),
			assert_message(Conn, RemainingAssertions, NewReports);
		
		{publish, OtherTopic, OtherPayload} ->
			lager:debug("Unexpected message on mqtt: ~p: ~p", [OtherTopic, OtherPayload]),
			assert_message(Conn, Assertions, Reports);
		
		{mqttc, Conn, disconnected} ->
			lager:warning("Mqttc connection lost!"),
			{error, mqttc_disconnected};
		
		{'DOWN', _Ref, process, TestActionPid, normal} ->
			lager:debug("The test action process ~p finished executing", [TestActionPid]),
			assert_message(Conn, Assertions, Reports);
		
		{'DOWN', _Ref, process, TestActionPid, _} ->
			lager:warning("The test action process ~p crashed!", [TestActionPid]),
			{error, test_action_crashed};
		
		Message ->
			lager:warning("Unexpected message: ~p~n", [Message]),
			assert_message(Conn, Assertions, Reports)
	end.

get_assertion({Topic, ExpectedPayload, AssertionMessage}) ->
	get_assertion({Topic, ExpectedPayload, AssertionMessage, ?DEFAULT_TIMEOUT});

get_assertion(Assertion) ->
	Assertion.

save_report(AssertionMessage, Reports) ->
	NewReports = [{AssertionMessage, success} | Reports],
	NewReports.

report(Reports) ->
	lager:debug("Printing reports now: ~p~n", [Reports]),
	%%TODO: print reports

	ReportedResults = [Result || {_Message, Result} <- Reports],
	%% ideally [success, success, ... , success]
	ReportedResults.
