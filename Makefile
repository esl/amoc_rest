.PHONY: all test generate validate compile console dialyzer xref

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

compile:
	./rebar3 compile

console:
	./rebar3 shell --apps=amoc_rest

dialyzer:
	./rebar3 dialyzer

xref:
	./rebar3 xref
