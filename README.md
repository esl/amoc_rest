# amoc_rest
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
