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
	FormattedAssertions = lists:map(fun get_assertion/1, Assertions),	%% apply default timeout
	
	Conn = emqttc_utils:connect_mqttc(MqttAddress),
	wait_until_connected(Conn, FormattedAssertions),
	
	TestAction = spawn_monitor(fun() -> ActionToTest(Conn) end),
	lager:debug("Test process spawned at ~p", [TestAction]),
	
	assert_message(Conn, FormattedAssertions, [], no_timer).


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


assert_message(_Conn, [], Reports, no_timer) ->
	report(Reports);

assert_message(Conn, Assertions, Reports, OldTimerProc) ->
	lager:debug("Calling assert, remaining assertions: ~p~n", [Assertions]),
	[CurrentAssertion | RemainingAssertions] = Assertions,
	{Topic, ExpectedPayload, AssertionMessage, TimeOut} = CurrentAssertion,
	
	TimerProc = start_timer_if_not_alive(OldTimerProc, TimeOut),
	
	receive
		{publish, Topic, ExpectedPayload} ->
			exit(TimerProc, assertion_success),	%% stopping timeout
			lager:debug("Assertion succeeded on topic ~p with payload ~p", 
						[Topic, ExpectedPayload]),
			NewReports = save_report(AssertionMessage, Reports, success),
			assert_message(Conn, RemainingAssertions, NewReports, no_timer);
		
		{publish, OtherTopic, OtherPayload} ->
			lager:debug("Unexpected message on mqtt: ~p: ~p", [OtherTopic, OtherPayload]),
			assert_message(Conn, Assertions, Reports, TimerProc);
		
		{assertion_timeout, TimerProc} ->
			lager:debug("Timeout message received from ~p", [TimerProc]),
			NewReports = save_report(AssertionMessage, Reports, failure),
			assert_message(Conn, RemainingAssertions, NewReports, no_timer);
		
		{mqttc, Conn, disconnected} ->
			lager:warning("Mqttc connection lost!"),
			{error, mqttc_disconnected};
		
		{'DOWN', _Ref, process, TestActionPid, normal} ->
			lager:debug("The test action process ~p finished executing", [TestActionPid]),
			assert_message(Conn, Assertions, Reports, TimerProc);
		
		{'DOWN', _Ref, process, TestActionPid, _} ->
			lager:warning("The test action process ~p crashed!", [TestActionPid]),
			{error, test_action_crashed};
		
		Message ->
			lager:warning("Unexpected message: ~p~n", [Message]),
			assert_message(Conn, Assertions, Reports, TimerProc)
	end.


get_assertion({Topic, ExpectedPayload, AssertionMessage}) ->
	get_assertion({Topic, ExpectedPayload, AssertionMessage, ?DEFAULT_TIMEOUT});

get_assertion(Assertion) ->
	Assertion.


start_timer_if_not_alive(no_timer, TimeOut) ->
	start_timer(TimeOut);

start_timer_if_not_alive(TimerProc, _TimeOut) ->
	TimerProc.
	
start_timer(TimeOut) ->
	HostProc = self(),
	_Pid = spawn(fun() -> timer:sleep(TimeOut),
						  HostProc ! {assertion_timeout, self()},
						  lager:debug("Assertion timeout after ~p ms", [TimeOut])
				 end).


save_report(AssertionMessage, Reports, Evaluation) ->
	_NewReports = [{AssertionMessage, Evaluation} | Reports].


report(Reports) ->
	lager:debug("Printing reports now: ~p~n", [Reports]),
	%%TODO: print reports

	_ReportedResults = [Result || {_Message, Result} <- Reports].
	%% ideally [success, success, ... , success]
