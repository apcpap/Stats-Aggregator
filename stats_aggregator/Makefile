all: deps compile

compile:
	rebar compile

deps:
	rebar get-deps

clean:
	rebar clean

test:
	CONFIG=$(PWD)/priv/app.config rebar skip_deps=true eunit

start:
	CONFIG=$(PWD)/priv/app.config erl -pz ebin deps/*/ebin -s stats_aggregator_server
