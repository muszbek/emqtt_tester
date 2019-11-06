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
	ValidAssertions = lists:map(fun validate_assertion/1, Assertions),	%% apply default timeout
	
	Conn = emqttc_utils:connect_mqttc(MqttAddress),
	wait_until_connected(Conn, ValidAssertions),
	
	TestAction = spawn_monitor(fun() -> ActionToTest(Conn) end),
	lager:debug("Test process spawned at ~p", [TestAction]),
	
	assert_message(Conn, ValidAssertions, [], no_timer).


%% ====================================================================
%% Internal functions
%% ====================================================================

validate_assertion({Topic, ExpectedPayload, AssertionMessage}) ->
	validate_assertion({Topic, ExpectedPayload, AssertionMessage, ?DEFAULT_TIMEOUT});

validate_assertion({Topic, Payload, Message, Timeout}) ->
	ValidatedTopic = case Topic of
						 T when is_binary(T) -> Topic;
						 T -> list_to_binary(T)
					 end,
	
	ValidatedPayload = case Payload of 
						   P when is_binary(P) -> Payload;
						   P -> list_to_binary(P)
					   end,
	
	{ValidatedTopic, ValidatedPayload, Message, Timeout}.


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


assert_message(Conn, [], Reports, no_timer) ->
	%% this clause is called when there are no more assertions left
	emqttc_utils:disconnect_mqttc(Conn),
	report(Reports);

assert_message(Conn, Assertions, Reports, OldTimerProc) ->
	lager:debug("Calling assert, remaining assertions: ~p~n", [Assertions]),
	[CurrentAssertion | RemainingAssertions] = Assertions,
	{Topic, ExpectedPayload, AssertionMessage, TimeOut} = CurrentAssertion,
	
	TimerProc = start_timer_if_not_alive(OldTimerProc, TimeOut),
	
	receive
		{publish, Topic, Payload} ->
			case binary:match(Payload, ExpectedPayload) of
				nomatch ->
					lager:debug("Unexpected message on mqtt: ~p: ~p", [Topic, Payload]),
					assert_message(Conn, Assertions, Reports, TimerProc);
				
				_ ->
					exit(TimerProc, assertion_success),	%% stopping timeout
					lager:debug("Assertion succeeded on topic ~p with payload ~p", 
								[Topic, ExpectedPayload]),
					NewReports = save_report(AssertionMessage, Reports, success),
					assert_message(Conn, RemainingAssertions, NewReports, no_timer)
			end;
		
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

report(ReverseReports) ->
	Reports = lists:reverse(ReverseReports),
	lager:debug("Printing reports now: ~p~n", [Reports]),
	lists:map(fun print_report/1, Reports),
	
	_ReportedResults = [Result || {_Message, Result} <- Reports].
	%% ideally [success, success, ... , success]

print_report(Report) ->
	{AssertionMessage, Evaluation} = Report,
	lager:info("--> ~p : ~p", [AssertionMessage, Evaluation]).
