-module(emqtt_tester).
-compile([{parse_transform, lager_transform}]).

-export([run/3]).

%% MqttAddress: {Host, Port, User, Pw}
%% Assertions: [{Topic :: bin, ExpectedPayload :: bin, AssertionMessage :: list(string)}]
%% ActionToTest: fun (Conn) -> ok/error
run(MqttAddress, Assertions, ActionToTest) ->
	Conn = emqttc_utils:connect_mqttc(MqttAddress),
	
	TestActionPid = spawn(fun() -> ActionToTest(Conn) end),
	lager:debug("Test process spawned at ~p", [TestActionPid]),
	
	listenOnMqtt(Conn, Assertions).


listenOnMqtt(Conn, Assertions) ->
	receive
		{mqttc, Conn, connected} ->
			lager:debug("Emqttc connected: ~p", [Conn]),
			emqttc_utils:subscribe_to_assertions(Conn, Assertions),
			
			assert_message(Conn, Assertions, []);
		
		Message ->
			lager:warning("Unexpected message: ~p~n", [Message])
		%% emqttc doesn't give any messages on timeout by default
	end.


assert_message(Conn, [], Reports) ->
	report(Reports);

assert_message(Conn, Assertions, Reports) ->
	lager:debug("Calling assert, remaining assertions: ~p~n", [Assertions]),
	[CurrentAssertion | RemainingAssertions] = Assertions,
	{Topic, ExpectedPayload, AssertionMessage} = CurrentAssertion,
	
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
			lager:warning("Mqttc connection lost!");
		
		Message ->
			lager:warning("Unexpected message: ~p~n", [Message]),
			assert_message(Conn, Assertions, Reports)
	end.

save_report(AssertionMessage, Reports) ->
	NewReports = [{AssertionMessage, success} | Reports],
	NewReports.

report(Reports) ->
	lager:debug("Printing reports now: ~p~n", [Reports]),
	ok.
