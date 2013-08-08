REBAR = rebar

all: deps compile

deps:
	$(REBAR) get-deps

compile:
	$(REBAR) compile 

tests: 
	$(REBAR) -C rebar.test.config get-deps compile
	ERL_FLAGS="-config app.config" $(REBAR) -C rebar.test.config skip_deps=true eunit 

clean:
	$(REBAR) clean 

start:
	erl -config app.config -pa ebin deps/*/ebin -s vis_request \
    	-eval "io:format(\"Point your browser at http://localhost:8080/ to use a simple websocket client~n\")."
