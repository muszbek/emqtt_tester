-module(emqtt_tester).
-compile([{parse_transform, lager_transform}]).

-export([run/3]).

%% MqttAddress: {Host, Port, User, Pw}
%% Assertions: [{Topic :: bin, ExpectedPayload :: bin, AssertionMessage :: list(string)}]
%% ActionToTest: fun (Conn) -> ok/error
run(MqttAddress, Assertions, ActionToTest) ->
	Conn = emqttc_utils:connect_mqttc(MqttAddress),
	
	TestActionPid = spawn(fun() -> ActionToTest(Conn) end),
	
	listenOnMqtt(Conn, Assertions),
	
	ok.


listenOnMqtt(Conn, Assertions) ->
	receive
		{mqttc, Conn, connected} ->
			lager:debug("Emqttc connected: ~p", [Conn]),
			emqttc_utils:subscribeToAssertions(Conn, Assertions),
			assertMessage(Conn, Assertions, []);
		
		Message ->
			lager:warning("Unexpected message: ~p~n", [Message])
		%% emqttc doesn't give any messages on timeout by default
	end.


assertMessage(Conn, [], Reports) ->
	report(Reports);

assertMessage(Conn, Assertions, Reports) ->
	lager:debug("Calling assert, remaining assertions: ~p~n", [Assertions]),
	[CurrentAssertion | RemainingAssertions] = Assertions,
	{Topic, ExpectedPayload, AssertionMessage} = CurrentAssertion,
	
	receive
		{publish, Topic, ExpectedPayload} ->
			lager:debug("Assertion succeeded on topic ~p with payload ~p", 
						[Topic, ExpectedPayload]),
			NewReports = saveReport(AssertionMessage, Reports),
			assertMessage(Conn, RemainingAssertions, NewReports);
		
		{publish, OtherTopic, OtherPayload} ->
			lager:debug("Unexpected message on mqtt: ~p: ~p", [OtherTopic, OtherPayload]),
			assertMessage(Conn, Assertions, Reports);
		
		{mqttc, Conn, disconnected} ->
			lager:warning("Mqttc connection lost!");
		
		Message ->
			lager:warning("Unexpected message: ~p~n", [Message]),
			assertMessage(Conn, Assertions, Reports)
	end.

saveReport(AssertionMessage, Reports) ->
	NewReports = [{AssertionMessage, success} | Reports],
	NewReports.

report(Reports) ->
	lager:debug("Printing reports now: ~p~n", [Reports]),
	ok.
