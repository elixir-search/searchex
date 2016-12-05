defmodule ExMake2 do
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
  - DAG processing topologies
  - bottom-up processing direction

  ### How it works

  The Parent module calls the `chain` function on the Child module.

  An `ExMake` behavior uses three callbacks:

  1. `chain_validations(args)` returns a list of validation functions.
  2. `chain_children(args)` a list of chained children to run.
  4. `chain_generate(args, child_state)` Content generation function.

  `chain`, `chain_action_when_fresh`, `chain_action_when_stale` all return
  one of the following tuples:
  - `{:ok}`
  - `{:ok, child_state}`
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
   5. The hash keys of the child data is looked up in the LRU cache.  If a cache
      value is found, it is returned.
   6. Otherwise, the `chain_generate` function is called.

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

  @doc "Start the cache service"
  def start_cache do
    "TBD"
  end

  @doc "Save the cache data to disk"
  def save_cache do
    "TBD"
  end

  @doc """
  Return a timestamp for a specific directory.

  Timestamp is the most recently modified file under the directory.
  """
  def dirpath_timestamp(dirpath) do
    Path.expand(dirpath)
    |> Searchex.Util.File.ls_r
    |> Enum.map(fn(path) -> filepath_timestamp(path) end)
    |> Enum.max
  end

  @doc """
  Return a timestamp for a list of directories.

  Timestamp is the most recently modified file for all sub-directories.
  """
  def dirlist_timestamp(dirlist) do
    dirlist
    |> Enum.map(fn(x) -> Path.expand(x) end)
    |> Enum.map(fn(x) -> dirpath_timestamp(x) end)
    |> newest
  end

  @doc """
  Timestamp 'newer' comparison predicate

  Example:

      iex> {{2012,2,5},{12,12,12}} |> is_newer_than? {{2014,9,9},{4,4,4}}
      false
  """
  def is_newer_than?(ts1, ts2), do: ts1 >= ts2

  @doc """
  Timestamp 'older' comparison predicate

  Example:

      iex> {{2012,2,5},{12,12,12}} |> is_older_than? {{2014,9,9},{4,4,4}}
      true
  """
  def is_older_than?(ts1, ts2), do: ts1  < ts2

  @doc "Returns the newest timestamp in a list."
  def newest(enum), do: Enum.max(enum)

  @doc "Returns the oldest timestamp in a list."
  def oldest(enum), do: Enum.min(enum)

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

  @doc false
  # Takes an array of `chained children`. Returns a tuple.
  # `{:error, [msg]}`, `{:stale, [val]}`, or `{:fresh, [val]}`
  # An `:error` is returned if only one parent has an error.
  # The `:stale` atom is returned unless all parents are `:fresh`
  # Return vals are in order of `chained parents`.
  defp chain_and_check(children, lcl_ts) when is_list(children) do
    Enum.reduce children, {:fresh, []}, fn
      ({:error, msg}  , {:error, _tmp}) -> {:error, msg ++ [msg]}
      ({:error, msg}  , _acc          ) -> {:error, [msg]}
      ({:ok, _ts}     , {:stale, vlst}) -> {:stale, vlst ++ [nil]}
      ({:ok, _ts, val}, {:stale, vlst}) -> {:stale, vlst ++ [val]}
      ({:ok, ts}      , {:fresh, vlst}) -> {check(ts, lcl_ts), vlst ++ [nil]}
      ({:ok, ts, val} , {:fresh, vlst}) -> {check(ts, lcl_ts), vlst ++ [val]}
    end
  end

  defp chain_and_check(child, lcl_timestamp) when is_function(child),
  do: chain_and_check(child.(), lcl_timestamp)

  defp chain_and_check(child, lcl_timestamp) when is_function(lcl_timestamp),
  do: chain_and_check(child, lcl_timestamp.())

  defp chain_and_check(child, lcl_timestamp),
  do: chain_and_check([child], lcl_timestamp)

  @doc false
  # If `child_timestamp` is newer than `lcl_timestamp,
  # return `:stale`, otherwise `:fresh`.
  defp check(child_timestamp, lcl_timestamp) when is_tuple(lcl_timestamp) do
    if is?(child_timestamp, newer_than: lcl_timestamp),do: :stale, else: :fresh
  end

  defp check(child_timestamp, lcl_timestamp) when is_function(lcl_timestamp),
  do: check(child_timestamp, lcl_timestamp.())

  @doc false
  # Run all chained children.
  # If there are any errors, return them.
  # When fresh, perform action (return a cached value)
  # When stale, perform action (generate a new returned value)
  def perform_action(params) do
    case chain_and_check(chain_children) do
      {:error , msgs }       -> {:error, msgs}
      {:fresh , child_state} -> params.action_when_fresh.(child_state)
      {:stale , child_state} -> params.action_when_stale.(child_state)
    end
  end

  defmacro __using__(_opts) do
    quote do

      @doc false
      def chain_validations(_args), do: []

      @doc false
      def chain_children(_args), do: []

      @doc false
      def chain_generate(_args, _child_state), do: nil

      @doc false
      def chain(args) do
        case ExMake2.check_validations(chain_validations) do
          {:error, msgs} -> {:error, msgs}
          _              -> ExMake2.perform_action(params)
        end
      end

      import ExMake
      @behaviour ExMake

      defoverridable [chain_validations: 1, chain_children: 1, chain_generate: 2]

    end
  end
end
