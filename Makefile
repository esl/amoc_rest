.PHONY: all test generate validate compile console dialyzer xref

REBARVER = 3.13.2
ifeq ($(OTPVER),24.0)
	REBARVER = 3.15.1
endif

OutputDir ?= tmp

define openapi-generator
  docker run --rm -v "${PWD}:/local" -w "/local" \
             openapitools/openapi-generator-cli:v4.3.1
endef

all: compile

test: validate compile dialyzer xref generate
	diff -sq priv/openapi.json "$(OutputDir)/priv/openapi.json"

generate:
	$(openapi-generator) generate \
	  -g erlang-server -i openapi.yaml -o "$(OutputDir)" \
	  --additional-properties=packageName=amoc_rest

validate:
	$(openapi-generator) validate -i openapi.yaml

compile: rebar3
	./rebar3 compile

console: rebar3
	./rebar3 shell --apps=amoc_rest

rebar3:
	wget https://github.com/erlang/rebar3/releases/download/${REBARVER}/rebar3 &&\
	chmod u+x rebar3

dialyzer: rebar3
	./rebar3 dialyzer

xref: rebar3
	./rebar3 xref
