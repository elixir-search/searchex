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

  `chain` returns one of:`{:ok, {state, digest}}` | `{:error, message}`

  A validation function returns one of: `{:ok}` | `{:error, message}`

  `chain_validations` returns: `{:ok}` | `{:error, [messages]}`

  `chain_children` returns: `[ list of chain returns ]`

  `chain_generate` returns: the state - any elixir term

  Here is the `chain` execution sequence:
  1. All the validations are run.  If one or more validations fail, `{:error,
     [messages]}` is returned.
  2. The `child` modules are all invoked.
  3. If one or more of the child modules fails, error messages are returned.
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

  @doc "Generates a digest for a term"
  def term_digest(term) do
    digest = term |> :erlang.term_to_binary |> Searchex.Util.String.digest(10)
    {term, digest}
  end

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
    Enum.reduce validations, {:ok}, fn(x, acc) -> reduce_val(x, acc) end
  end

  def check_validations(validations) when is_function(validations) do
    check_validations(validations.())
  end

  def check_validations(validation), do: check_validations([validation])

  def reduce_val(func, acc) when is_function(func), do: reduce_val(func.(), acc)
  def reduce_val({:error, msg}, {:ok}        ), do: {:error, [msg]       }
  def reduce_val({:error, msg}, {:error, lst}), do: {:error, lst ++ [msg]}
  def reduce_val({:ok}        , {:error, lst}), do: {:error, lst         }
  def reduce_val({:ok   , _xx}, {:error, lst}), do: {:error, lst         }
  def reduce_val({:ok}        , {:ok}        ), do: {:ok                 }
  def reduce_val({:ok}        , {:ok   , lst}), do: {:ok   , lst         }
  def reduce_val({:ok   , val}, {:ok}        ), do: {:ok   , [val]       }
  def reduce_val({:ok   , val}, {:ok   , lst}), do: {:ok   , lst ++ [val]}

  @doc false
  # Takes an array of `chained children`. Returns a tuple.
  # `{:error, [msg]}` or `{:ok, [val]}`
  # An `:error` is returned if one or more child has an error.
  # Return vals are in order of `chained children`.
  def chain_and_check(children) when is_list(children) do
    Enum.reduce children, {:ok, []}, fn
      ({:error, msg}, {:error, ele}) -> {:error, ele ++ [msg]}
      ({:error, msg}, _acc         ) -> {:error, [msg]}
      ({:ok}        , {:ok, vlst}  ) -> {:ok   , vlst ++ [nil]}
      ({:ok   , val}, {:ok, vlst}  ) -> {:ok   , vlst ++ [val]}
    end
  end

  def chain_and_check(child) when is_function(child),
  do: chain_and_check(child.())

  def chain_and_check(child),
  do: chain_and_check([child])

  @doc false
  # look up cached value
  # if not found, regenerate
  def current_action(args, child_state, module) do
    TIO.inspect "CURRENT", color: "RED"
    child_key = Enum.reduce(child_state, "", fn(x, acc) -> acc <> elem(x, 1) end)
    TIO.inspect child_key, color: "CYAN"
    if val = TIO.inspect(ExCache.get_cache(child_key), color: "RED") do
      val
    else
      TIO.inspect :A, color: "GREEN"
      val = module.chain_generate(args, child_state)
      TIO.inspect [B: val], color: "GREEN"
      return_val = val |> term_digest
      TIO.inspect [C: return_val], color: "GREEN"
      ExCache.put_cache(child_key, return_val)
      TIO.inspect :D, color: "GREEN"
      {:ok, return_val}
    end
  end

  @doc false
  # Run all chained children.
  # If there are any errors, return them.
  # Otherwise perform the action
  def child_actions(args, module) do
    case TIO.inspect(chain_and_check(module.chain_children(args)), color: "GREEN") do
      {:error , msgs }       -> {:error, msgs}
      {:ok    , child_state} -> TIO.inspect(current_action(args, child_state, module), color: "MAGENTA")
    end
  end

  defmacro __using__(_opts) do
    quote do

      @doc false
      def chain_validations(_args), do: []

      @doc false
      def chain_children(_args), do: []

      @doc false
      def chain_generate(_args, _child_state), do: :notimpl

      @doc false
      def chain(args) do
        case ExMake.check_validations(__MODULE__.chain_validations(args)) do
          {:error, msgs} -> {:error, msgs}
          _              -> ExMake.child_actions(args, __MODULE__)
        end
      end

      def chain, do: chain(:ok)

      import ExMake
      @behaviour ExMake

      defoverridable [chain_validations: 1,
                      chain_children: 1,
                      chain_generate: 2]

    end
  end
end
