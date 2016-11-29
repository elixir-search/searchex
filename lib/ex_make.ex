defmodule ExMake do
  @moduledoc """
  Generic Make behavior for Elixir

  The idea:
  - support make-like behavior
  - multiple processes are joined together in a dependency chain
  - processess can create intermediate products which are combined together
  - the goal is to prevent re-generation of intermediate products unless necessary
  - maintain compatibility with Elixir's concurrent / distributed features

  ExMake works by comparing timestamps - which are always a tuple of six integers:
  `{{year, month, day}, {hour, min, sec}}`

  For more precision, add milliseconds (thousandths of a second)
  to your timestamp, like this:
  `{{year, month, day}, {hour, min, sec, msec}}`

  Important: `year` should always be four digits!!

  Example:

  ```elixir
  defmodule Target do
    def perform_action do
      {:ok, parent_timestamp, parent_val} = Parent.chain(<params>)
      case local_timestamp < parent_timestamp do
        true  -> {:ok, timestamp_now, generate_new_return_val(parent_val)}
        false -> {:ok, local_timestamp, return_cached_val}
      end
    end
  end

  defmodule Parent do
    use ExMake

    def handle_chain(args) do
      {:ok, parent_timestamp, parent_val} = GrandParent.chain(args)
      case local_timestamp < parent_timestamp do
        true  -> {:ok, timestamp_now, generate_new_return_val(parent_val)}
        false -> {:ok, local_timestamp, return_cached_val}
      end
    end
  end

  defmodule GrandParent do
    use ExMake

    def handle_chain(args) do
      {:ok, local_timestamp}
    end
  end
  ```

  This module implements a single callback: `handle_chain`.

  The calling process always uses the `chain` method.

  The called process always uses the `handle_chain` method.

  `handle_chain` always returns one of the following tuples:
  - {:ok, timestamp}
  - {:ok, timestamp, parent_val}
  - {:error, message}

  Note: timestamps can represent events like:
  - file modification date
  - directory modification date (recursive search)
  - last compilation date
  - last GenServer state change
  - last call to a third-party API
  - an http 'last-modified' header
  - etc.

  The calling process is responsible for comparing the parent and
  local timestamps and taking appropriate actions.
  """

  @compile_time System.cmd("date", ["+%Y %m %d %H %M %S"])

  @callback handle_chain(tuple) :: tuple

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
    |> Enum.max
  end

  @doc "Return a timestamp with the compile time"
  def compile_timestamp do
    {time, _} = @compile_time
    [y,m,d,hh,mm,ss] = time |> String.split |> Enum.map(&String.to_integer/1)
    {{y,m,d},{hh,mm,ss}}
  end

  defmacro __using__(_opts) do
    quote do
      def chain(args) do
        # TODO error checking here
        result = handle_chain(args)
        # TODO error checking here
        result
      end

      import ExMake
      @behaviour ExMake
    end
  end
end
