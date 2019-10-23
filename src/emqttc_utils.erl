%% @author tmuszbek
%% @doc @todo Add description to emqttc_utils.


-module(emqttc_utils).
-compile([{parse_transform, lager_transform}]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([subscribeToAssertions/2, connect_mqttc/1]).


subscribeToAssertions(_Conn, []) ->
	lager:debug("All assertion topics have been subscribed"),
	ok;

subscribeToAssertions(Conn, Assertions) ->
	[CurrentAssertion | RemainingAssertions] = Assertions,
	subscribeAssertion(Conn, CurrentAssertion),
	
	subscribeToAssertions(Conn, RemainingAssertions).


connect_mqttc({BrokerHost, BrokerPort, BrokerUser, BrokerPasswd})->
	connect_mqttc({BrokerHost, BrokerPort, BrokerUser, BrokerPasswd, << "Mqtt_tester" >>});

connect_mqttc({BrokerHost, BrokerPort, BrokerUser, BrokerPasswd, MyId})->
    lager:debug("Launching emqttc"),

    {ok, Conn} = emqttc:start_link([{host, BrokerHost},
				                       {port, BrokerPort},
				                       {username, list_to_binary(BrokerUser)},
				                       {password, list_to_binary(BrokerPasswd)},
				                       {client_id,  MyId},
				                       {logger, {lager, warning}},
				                       {auto_resub},
				                       {reconnect, {5, 60, 100}}
				                      ]),
	Conn.

%% ====================================================================
%% Internal functions
%% ====================================================================

subscribeAssertion(Conn, {Topic, _Payload, _Message}) ->
	lager:debug("Subscribing to topic ~p", [Topic]),
	emqttc:subscribe(Conn, Topic, 0).

unsubscribeAssertion(Conn, {Topic, _Payload, _Message}) ->
	lager:debug("Unsubscribing from topic ~p", [Topic]),
	emqttc:unsubscribe(Conn, Topic).
