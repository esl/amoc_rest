# amoc_rest [![](https://github.com/esl/amoc_rest/workflows/CI/badge.svg)](https://github.com/esl/amoc_rest/actions?query=workflow%3ACI)

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
    ModuleHandler = openapi_callbacks, %% Module implementing the callbacks behaviour
    TransportOpts = #{socket_opts => [{ip, {0, 0, 0, 0}}, {port, Port}]},
    ProtocolOpts = #{metrics_callback => fun prometheus_cowboy2_instrumenter:observe/1,
                     stream_handlers => [cowboy_metrics_h]},
    amoc_rest_server:start(
        openapi_http_server,
        #{
            transport => tcp,
            transport_opts => TransportOpts,
            protocol_opts => ProtocolOpts,
            logic_handler => ModuleHandler
        }
    ).
  ```

## swagger-ui integration
* the `dist` version of the swagger-ui is integrated under `/api-docs/` path (e.g. http://localhost:4000/api-docs/)
* the online version of the documentation is also available [here](https://esl.github.io/amoc_rest/) (w/o possibility of execution)
