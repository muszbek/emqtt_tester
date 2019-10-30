%% @author tmuszbek
%% @doc @todo Add description to emqttc_utils.


-module(emqttc_utils).
-compile([{parse_transform, lager_transform}]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([subscribe_to_assertions/2, connect_mqttc/1, disconnect_mqttc/1]).


subscribe_to_assertions(_Conn, []) ->
	lager:debug("All assertion topics have been subscribed"),
	ok;

subscribe_to_assertions(Conn, Assertions) ->
	[CurrentAssertion | RemainingAssertions] = Assertions,
	subscribe_assertion(Conn, CurrentAssertion),
	
	subscribe_to_assertions(Conn, RemainingAssertions).


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

disconnect_mqttc(Conn) ->
	lager:debug("Disconnecting from emqttc"),
	emqttc:disconnect(Conn).


%% ====================================================================
%% Internal functions
%% ====================================================================

subscribe_assertion(Conn, {Topic, _Payload, _Message, _TimeOut}) ->
	lager:debug("Subscribing to topic ~p", [Topic]),
	emqttc:subscribe(Conn, Topic, 0).

unsubscribe_assertion(Conn, {Topic, _Payload, _Message, _TimeOut}) ->
	lager:debug("Unsubscribing from topic ~p", [Topic]),
	emqttc:unsubscribe(Conn, Topic).
