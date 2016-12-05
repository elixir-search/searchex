defmodule ExMake do
  @moduledoc """
  Generic Make behavior for Elixir

  ### Overview

  `ExMake` Enables:
  - make-like behavior
  - multiple modules joined together in a Parent > Child dependency chain
  - compatibility with Elixir's concurrent / distributed features

  `ExMake` is different than the standard Elixir pipeline:
  - caching to prevent unnecessary re-generation of intermediate products
  - validations and error checking at every step of the chain
  - top-down process definition, DAG topologies

  ### How it works

  The Parent module calls the `chain` function on the Child module.

  An `ExMake` behavior uses three callbacks:

  1. `chain_validations(args)` returns a list of validation functions.
  2. `chain_children(args)` a list of chained children to run.
  4. `chain_generate(args, child_state)` Content generation function.

  `chain`, and `chain_generate` return one of the following tuples:
  - `{:ok}`
  - `{:ok, {child_state, digest}}`
  - `{:error, message}`

  A validation function returns one of:
  - `{:ok}`
  - `{:error, message}`

  Here is the `chain` execution sequence:
  1. All the validations are run.  If one or more validations fail, `{:error,
  [messages]}` is returned.
  2. The `child` modules are all invoked.
  3. If one or more of the child modules fails, the error message is returned.
  4. Otherwise the child returns data.
  5. The a cache value is looked up in the LRU cache.  The key is the digest
  of the child_state(s).
  6. If a cache value is not found, the `chain_generate` function is called,
  and the newly-generated data is stored in the LRU cache using the
  child-digest as the key.

  ### Caching

  `ExMake` uses an ETS-based LRU cache at every step of the generation chain.

  Cache keys are content digests, generated and expired automatically.

  ### Example

  ```elixir
  defmodule TestModule do
    use ExMake

    def api_call(args) do
      chain {:test, args}   
    end

    def chain_validations({:test, args}) do
      [ validation1(args), validation2(args)]
    end

    def chain_children({:test, args}) do
      [ Child1.chain({:ctx1, args}), Child2.chain({:ctx2, args}) ]
    end

    def chain_generate({:test, args}, child_state) do
      {:ok, new_state}
    end
  end
  ```
  """

  # TODO: define types
  @callback chain_validations(tuple)   :: list
  @callback chain_children(tuple)      :: list
  @callback chain_generate(tuple, any) :: tuple

  @doc """
  Checks a set of validation functions

  If all validation functions pass, return `{:ok}`.

  If one or more validation functions fail, return
  `{:error, [list of error messages]}`

  NOTE: validation functions returns one of:
  {:ok}
  {:error, msg}
  """
  def check_validations(validations) when is_list(validations) do
    Enum.reduce validations, {:ok}, fn
      ({:error, msg}, {:ok}        ) -> {:error, [msg]       }
      ({:error, msg}, {:error, lst}) -> {:error, lst ++ [msg]}
      ({:ok}        , {:error, lst}) -> {:error, lst         }
      ({:ok}        , {:ok}        ) -> {:ok}
    end
  end

  def check_validations(validations) when is_function(validations) do
    check_validations(validations.())
  end

  defmacro __using__(_opts) do
    quote do

      @doc false
      # Takes an array of `chained children`. Returns a tuple.
      # `{:error, [msg]}` or `{:ok, [val]}`
      # An `:error` is returned if only one parent has an error.
      # Return vals are in order of `chained parents`.
      defp chain_and_check(children) when is_list(children) do
        Enum.reduce children, {:ok, []}, fn
          ({:error, msg}, {:error, ele}) -> {:error, ele ++ [msg]}
          ({:error, msg}, _acc         ) -> {:error, [msg]}
          ({:ok}        , {:ok, vlst}  ) -> {:ok   , vlst ++ [nil]}
          ({:ok   , val}, {:ok, vlst}  ) -> {:ok   , vlst ++ [val]}
        end
      end

      defp chain_and_check(child, lcl_timestamp) when is_function(child),
      do: chain_and_check(child.(), lcl_timestamp)

      defp chain_and_check(child, lcl_timestamp) when is_function(lcl_timestamp),
      do: chain_and_check(child, lcl_timestamp.())

      defp chain_and_check(child, lcl_timestamp),
      do: chain_and_check([child], lcl_timestamp)

      @doc false
      # look up cached value
      # if not found, regenerate
      defp perform_current_action(args, child_state) do
        digest_key = Enum.reduce(child_state, "", fn(x, acc) -> acc <> elem(x, 1) end)
        if val = ExCache.get_cache(digest_key) do
          val
        else
          val = generate_content(args, child_state)
          digest_key = val |> :erlang.term_to_binary |> ExCache.put_cache(val)
          {val, digest_key}
        end
      end

      @doc false
      # Run all chained children.
      # If there are any errors, return them.
      # When fresh, perform action (return a cached value)
      # When stale, perform action (generate a new returned value)
      defp perform_child_action(args) do
        case ExMake.chain_and_check(chain_children) do
          {:error , msgs }       -> {:error, msgs}
          {:ok    , child_state} -> perform_current_action(args, child_state)
        end
      end

      @doc false
      def chain_validations(_args), do: []

      @doc false
      def chain_children(_args), do: []

      @doc false
      def chain_generate(_args, _child_state), do: nil

      @doc false
      def chain(args) do
        case ExMake.check_validations(chain_validations) do
          {:error, msgs} -> {:error, msgs}
          _              -> ExMake.perform_child_action(args)
        end
      end

      import ExMake
      @behaviour ExMake

      defoverridable [chain_validations: 1, chain_children: 1, chain_generate: 2]

    end
  end
end
