.PHONY: generate validate

OutputDir ?= tmp

define openapi-generator
  docker run --rm -v "${PWD}:/local" -w "/local" \
             openapitools/openapi-generator-cli:v4.3.1
endef

generate:
	$(openapi-generator) generate \
	  -g erlang-server -i openapi.yaml -o "$(OutputDir)" \
	  --additional-properties=packageName=amoc_rest

validate:
	$(openapi-generator) validate -i openapi.yaml
