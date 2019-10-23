- be able to assert arrival of mqtt messages
- assertion is sequential (or not? maybe an option?)
- report assertions

- operated as a single function call, should be able to embed in eunit or use similarly
(figure out later how to run it nicely, completely automated with docker)

- run(list MqttConnDetails, list Assertions, func ActionThatTests (optional?))
- ActionThatTests takes the mqtt connection as argument