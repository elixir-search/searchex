defmodule ExMake do
  @moduledoc """
  Generic Make behavior for Elixir

  ### Overview
  
  - support make-like behavior
  - multiple modules are joined together in a Parent > Child dependency chain
  - modules can create intermediate cached products which can be combined
  - handle validations and error checking at every step of the chain
  - use caching to prevent unnecessary re-generation of intermediate products
  - maintain compatibility with Elixir's concurrent / distributed features

  ### Timestamps
  
  `ExMake` works by comparing timestamps - a tuple of six integers:
  `{{year, month, day}, {hour, min, sec}}`

  For more precision, add milliseconds (thousandths of a second)
  to your timestamp, like this: `{{year, month, day}, {hour, min, sec, msec}}`

  Important: `year` should be four digits!!

  ### How it works

  The Parent module calls the `chain` function on the Child module.

  An `ExMake` behavior requires five callbacks:

  1. `chain_validations(args)` returns a list of validation functions.
  2. `chain_children(args)` a list of chained children to run.
  3. `chain_lcl_timestamp(args)` a timestamp function to be used locally.
  4. `chain_action_when_fresh(args, child_state)` Runs when state is fresh.
  5. `chain_action_when_stale(args, child_state)` Child state is a list, one per child.

  `chain`, `chain_action_when_fresh`, `chain_action_when_stale` all return
  one of the following tuples:
  - `{:ok, timestamp}`
  - `{:ok, timestamp, child_state}`
  - `{:error, message}`

   A validation function returns one of:
   - `{:ok}`
   - `{:error, message}`

   Here is the `chain` execution sequence:
   1. All the validations are run.  If one or more validations fail, `{:error,
     [messages]}` is returned.
   2. The `child` modules are all invoked.
   3. If one or more of the child modules fails, the error message is returned.
   4. The timestamps of the child modules are compared to the timestamp of the
      current module.  If one or more of the child modules is newer than the
      current module, the current state is marked as `:stale`.
   5. If the current state is `:stale`, invoke `chain_action_when_stale`.
   6. If the current state is `:fresh`, invoke `chain_action_when_fresh`.

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

    def chain_lcl_timestamp({:test, args}) do
      timestamp_function(args)
    end

    def chain_action_when_fresh({:test, args}, child_state) do
      {:ok, timestamp_function(args), new_state}
    end

    def chain_action_when_stale({:test, args}, child_state) do
      {:ok, timestamp_function(args), new_state}
    end
  end
  ```
  ### Notes
  
  Note: timestamps can represent events like:
  - file modification date
  - directory modification date (recursive search)
  - last GenServer state change
  - last call to a third-party API
  - an http 'last-modified' header, etc.

  Note: intermediate state can be cached in any form you want:
  disk files, SQL / ETS / DETS / Mnesia databases, GenServer processes, etc.

  """

  # TODO: define types
  @callback chain_validations(tuple)            :: list
  @callback chain_children(tuple)               :: list
  @callback chain_lcl_timestamp(tuple)          :: tuple
  @callback chain_action_when_fresh(tuple, any) :: tuple
  @callback chain_action_when_stale(tuple, any) :: tuple

  @doc "Return the current timestamp."
  def timestamp_now do
    {time, _} = System.cmd("date", ["+%Y %m %d %H %M %S"])
    [y,m,d,hh,mm,ss] = time |> String.split |> Enum.map(&String.to_integer/1)
    {{y,m,d},{hh,mm,ss}}
  end

  @doc "Return a timestamp for a specific filepath."
  def filepath_timestamp(path) do
    epath = Path.expand(path)
    case File.stat(epath, time: :local) do
      {:ok, info} -> Map.get(info, :mtime)
      _           -> {{0,0,0},{0,0,0}}
    end
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
  Predicate to compare two timestamps.
  
  Uses `newer_than` / `older_than` labels.

  Examples:

      iex> is? {{2012,2,5},{12,12,12}}, newer_than: {{2014,9,9},{4,4,4}}
      false

      iex> is? {{2012,2,5},{12,12,12}}, older_than: {{2014,9,9},{4,4,4}}
      true
  """
  def is?(timestamp, args) do
    cond do
      args[:newer_than] -> timestamp >= args[:newer_than]
      args[:older_than] -> timestamp  < args[:older_than]
      true              -> false
    end
  end

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
    case chain_and_check(params.children, params.lcl_timestamp) do
      {:error , msg  }       -> {:error, msg}
      {:fresh , child_state} -> params.action_when_fresh.(child_state)
      {:stale , child_state} -> params.action_when_stale.(child_state)
    end
  end

  defmacro __using__(_opts) do
    quote do

      @doc false
      # links to all the callbacks in `ExMake`
      def handle_chain(args) do
        %{
          validations:       fn -> chain_validations(args)                                 end  ,
          children:          fn -> chain_children(args)                                    end  ,
          lcl_timestamp:     fn -> chain_lcl_timestamp(args)                               end  ,
          action_when_fresh: fn(child_state) -> chain_action_when_fresh(args, child_state) end  ,
          action_when_stale: fn(child_state) -> chain_action_when_stale(args, child_state) end
        }
      end

      @doc false
      # why is this function defined in the macro?
      # because it does a callback to `handle_chain`
      # which is defined in the host method.
      def chain(args) do
        params = handle_chain(args)
        case ExMake.check_validations(params.validations) do
          {:error, msgs} -> {:error, msgs}
          _              -> ExMake.perform_action(params)
        end
      end

      import ExMake
      @behaviour ExMake
    end
  end
end
