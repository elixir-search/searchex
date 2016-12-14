defmodule Util.TimeStamp do

  @moduledoc false
  # Utility functions for generating timestamps.

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
    |> Util.Ext.File.ls_r
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
  end
