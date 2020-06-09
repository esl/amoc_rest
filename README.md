# amoc_rest [![Build Status](https://travis-ci.org/esl/amoc_rest.svg?branch=master)](https://travis-ci.org/esl/amoc_rest)

The generated server code for AMOC REST API

## initial generation
* initial version of the project is generated using openapi-generator.

  ```bash
  make generate
  ```

## compilation and execution of the project
* compilation of the project can be done using this command:

  ```bash
  make compile
  ```

* to test the project you can run:

  ```bash
  make console
  ```

  and then, execute in the erlang shell:

  ```erlang
  ServerParams = #{ip => {0, 0, 0, 0}, port => 4000, net_opts => []}.
  amoc_rest_server:start(http_server, ServerParams).
  ```

## swagger-ui integration
* the `dist` version of the swagger-ui is integrated under `/api-docs/` path (e.g. http://localhost:4000/api-docs/)
* the online version of the documentation is also available [here](https://esl.github.io/amoc_rest/?v=7890862) (w/o possibility of execution)
