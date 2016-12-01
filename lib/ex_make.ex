defmodule ExMake do
  @moduledoc """
  Generic Make behavior for Elixir

  The idea:
  - support make-like behavior
  - multiple processes are joined together in a Parent>Child dependency chain
  - processess can create intermediate cached products which can be combined
  - the goal is to prevent unnecessary re-generation of intermediate products
  - maintain compatibility with Elixir's concurrent / distributed features

  ExMake works by comparing timestamps - a tuple of six integers:
  `{{year, month, day}, {hour, min, sec}}`

  For more precision, add milliseconds (thousandths of a second)
  to your timestamp, like this: `{{year, month, day}, {hour, min, sec, msec}}`

  Important: `year` should be four digits!!

  `ExMake` requires a single callback: `handle_chain`.

  The Parent module calls the `chain` function on the Child module.

  The Child module implements the `handle_chain` function.

  `chain` returns one of the following tuples:
  - {:ok, timestamp}
  - {:ok, timestamp, return_val}
  - {:error, message}

  `handle_chain` returns a map with the following keys:
   - validations: a list of validation functions
   - children: a list of `Method.chain` functions to call
   - lcl_timestamp: a timestamp function to be used locally
   - action_when_fresh: a function to run when current state is fresh
   - action_when_stale: function to run when current state is stale
   
   A validation function returns one of:
   - {:ok}
   - {:error, message}

   Here is the `chain` execution sequence:
   1. All the validations are run.  If one or more validations
      fail, `{:error, [messages]}` is returned.
   2. The `child` modules are all invoked.
   3. If one or more of the child modules fails, the error
      message is returned.
   4. The timestamps of the child modules are compared to the
      timestamp of the current module.  If one of the child
      modules is newer than the current module, the current
      state is marked as `:stale`.
   5. If the current state is `:stale`, the `action_when_stale`
      function is invoked.
   6. If the current state is `:fresh`, the `action_when_fresh`
      function is invoked.

  Note: timestamps can represent events like:
  - file modification date
  - directory modification date (recursive search)
  - last compilation date
  - last GenServer state change
  - last call to a third-party API
  - an http 'last-modified' header
  - etc.

  Note: intermediate state can be cached in any form you want:
  - disk files
  - SQL / ETS / DETS / Mnesia databases
  - GenServer processes
  - etc.

  Here's an example of `ExMake` in action:

  ```elixir
  defmodule TestModule do
    use ExMake
  
    def api_call(args) do
      chain {:test_run, args}   # runs on itself!
    end
  
    def handle_chain({:test_run, args})
      %{
          validations:       []                         ,
          children:          []                         ,
          lcl_timestamp:   &(calculate_timestamp(args)) ,
          action_when_fresh: &(gen_fresh(&1, args))     ,
          action_when_stale: &(gen_stale(&1, args))
        }
     end
  
    def perform_operation(params) do
      {:ok, parent_timestamp, parent_val} = Parent.chain(<params>)
      case lcl_timestamp < parent_timestamp do
        true  -> {:ok, timestamp_now, generate_new_return_val(parent_val)}
        false -> {:ok, lcl_timestamp, return_cached_val}
      end
    end
  end
  ```

  More Notes:
  - All `handle_chain` fields are optional.
  - If there are no children, the state is `:stale`, and
    `action_when_stale` is invoked.
  - You can implement more than one `handle_chain` function per module.
    Each implementation must match a unique tuple as the function argument.

   Pro Tip: Sometimes rather than linking to a child sub-module, it is simpler
   to stick a `chain` return value directly in the children list - like this:

   ```elixir
   def handle_chain({:test, args}) do
     %{children: [{:ok, {{2014, 05, 33}}, {{00,00,00}}}]}
   end
   ```
  """

  @callback handle_chain(tuple) :: map

  @doc "Return the current timestamp"
  def timestamp_now do
    {time, _} = System.cmd("date", ["+%Y %m %d %H %M %S"])
    [y,m,d,hh,mm,ss] = time |> String.split |> Enum.map(&String.to_integer/1)
    {{y,m,d},{hh,mm,ss}}
  end

  @doc "Return a timestamp for a specific filepath"
  def filepath_timestamp(path) do
    epath = Path.expand(path)
    {:ok, info} = File.stat(epath, time: :local)
    Map.get(info, :mtime)
  end

  @doc """
  Return a timestamp for a specific directory

  Timestamp is the most recently modified file under the directory.
  """
  def dirpath_timestamp(dirpath) do
    Path.expand(dirpath)
    |> Searchex.Util.File.ls_r
    |> Enum.map(fn(path) -> filepath_timestamp(path) end)
    |> Enum.max
  end

  @doc """
  Return a timestamp for a list of directories

  Timestamp is the most recently modified file for all sub-directories.
  """
  def dirlist_timestamp(dirlist) do
    dirlist
    |> Enum.map(fn(x) -> Path.expand(x) end)
    |> Enum.map(fn(x) -> dirpath_timestamp(x) end)
    |> newest
  end

  @doc """
  Predicate to compare two timestamps using `newer_than` / `older_than` labels

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

  @doc """
  For working with timestamps to make the function name 
  `Enum.max` more intention revealing
  """
  def newest(enum), do: Enum.max(enum)

  @doc """
  For working with timestamps to make the function name 
  `Enum.min` more intention revealing
  """
  def oldest(enum), do: Enum.min(enum)

  # -----------------------------------------------------
  
  @doc false
  # default params for `handle_chain`
  def default_params do
    %{
      validations:       []                       ,
      children:          []                       ,
      lcl_timestamp:     &timestamp_now/0         ,
      action_when_fresh: {:ok, &timestamp_now/0}  ,
      action_when_stale: {:ok, &timestamp_now/0}
    }
  end

  @doc false
  # NOTE: validation functions returns one of:
  # {:ok}
  # {:error, msg}
  def check_validations(validations, args) when is_list(validations) do
    DIO.inspect [VAL1: validations], color: "GREEN"
    DIO.inspect [VAL2: args], color: "GREEN"
    Enum.reduce validations, {:ok}, fn
      ({:error, msg}, {:ok}        ) -> {:error, [msg]       }
      ({:error, msg}, {:error, lst}) -> {:error, lst ++ [msg]}
      ({:ok}        , {:error, lst}) -> {:error, lst         }
      ({:ok}        , {:ok}        ) -> {:ok}
    end
  end

  def check_validations(validations, args) when is_function(validations) do
    DIO.inspect [VAL0: validations], color: "CYAN"
    check_validations(validations.(), args)
  end

  @doc false
  # Takes an array of `chained children`
  # Returns a tuple
  # `{:error, [msg]}`, `{:stale, [val]}`, or `{:fresh, [val]}`
  # An `:error` is returned if only one parent has an error.
  # The `:stale` atom is returned unless all parents are `:fresh`
  # Return vals are in order of `chained parents`.
  defp chain_and_check(children, lcl_ts) when is_list(children) do
    DIO.inspect [CHILD: children], color: "BLUE"
    DIO.inspect [LCLTS: lcl_ts], color: "BLUE"
    Enum.reduce children, {:fresh, []}, fn
      ({:error, msg}  , {:error, _tmp}) -> DIO.inspect {:error, msg ++ [msg]}             , color: "RED"
      ({:error, msg}  , _acc          ) -> DIO.inspect {:error, [msg]}                    , color: "RED"
      ({:ok, _ts}     , {:stale, vlst}) -> DIO.inspect {:stale, vlst ++ [nil]}            , color: "RED"
      ({:ok, _ts, val}, {:stale, vlst}) -> DIO.inspect {:stale, vlst ++ [val]}            , color: "RED"
      ({:ok, ts}      , {:fresh, vlst}) -> DIO.inspect {check(ts, lcl_ts), vlst ++ [nil]} , color: "RED"
      ({:ok, ts, val} , {:fresh, vlst}) -> DIO.inspect {check(ts, lcl_ts), vlst ++ [val]} , color: "RED"
    end
  end

  defp chain_and_check(child, lcl_timestamp) when is_function(child),
  do: chain_and_check(child.(), lcl_timestamp)

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
  def perform_action(params, _arg) do
    case chain_and_check(params.children, params.lcl_timestamp) do
      {:error , msg  }  -> {:error, msg}
      {:fresh , values} -> params.action_when_fresh.(values)
      {:stale , values} -> params.action_when_stale.(values)
    end
  end

  defmacro __using__(_opts) do
    quote do

      @doc false
      # why is this function defined in the macro?
      # because it does a callback to `handle_chain`
      # which is defined in the host method.
      def chain(args) do
        params = Map.merge(ExMake.default_params, handle_chain(args))
        DIO.inspect [BING: params], color: "BLUE"
        case DIO.inspect(ExMake.check_validations(params.validations, args), color: "MAGENTA") do
          {:error, msgs} -> {:error, msgs}
          _              -> ExMake.perform_action(params, args)
        end
      end

      import ExMake
      @behaviour ExMake
    end
  end
end
