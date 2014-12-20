start: compile
	erl -pa ./ebin -s gml_app start

compile:
	./rebar compile

test:
	./rebar eunit
