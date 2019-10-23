%% @author tmuszbek
%% @doc @todo Add description to emqtt_tester_test.


-module(emqtt_tester_test).
-include_lib("eunit/include/eunit.hrl").
-compile([{parse_transform, lager_transform}]).

-define(setup(F), {foreach, fun setup/0, fun cleanup/1, F}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTER DATA DEFINITIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(MQTT_ADDRESS, {"10.5.0.10", 1883, "odin", "odin42"}).
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
	ok.

cleanup(_) ->
	ok.

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%

