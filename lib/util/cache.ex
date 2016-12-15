defmodule Util.Cache do
  @moduledoc false
  # Simple LRU cache, with optional persistence to disk.

  @doc """
  Start the cache service

  Options are:
  - size - max number of items in cache
  - path - file to store the backup
  """
  def start(opts \\ []) do
    mopt = merged_opts(opts)
    LruCache.start_link(:ex_cache_ets, mopt[:size])
    if mopt[:path] do
      charpath = String.to_charlist(mopt[:path])
      :dets.open_file(:ex_cache_dets, [file: charpath])
      :ets.from_dets(:ex_cache_ets, :ex_cache_dets)
      :dets.close(:ex_cache_dets)
    end
    Util.GlobalState.set(:ex_cache, mopt)
    :ok
  end

  @doc "Stop the cache process"
  def stop do
    save
    if Process.whereis(:ex_cache_ets),  do: GenServer.stop(:ex_cache_ets)
    if Process.whereis(:ex_cache_dets), do: GenServer.stop(:ex_cache_dets)
  end

  defp merged_opts(opts) do
    [
      [size: 50, path: nil]                     ,
      Util.GlobalState.get(:ex_cache) || []     ,
      opts
    ]
    |> Enum.reduce([], fn(el, acc) -> Keyword.merge(acc, el) end)
  end

  @doc "Save the cache data to disk"
  def save do
    if path = Util.GlobalState.get(:ex_cache)[:path] do
      charpath = String.to_charlist(path)
      :dets.open_file(:ex_cache_dets, [file: charpath])
      :ets.to_dets(:ex_cache_ets, :ex_cache_dets)
      :dets.close(:ex_cache_dets)
    end
  end

  @doc "Get a value from the cache"
  def get_cache(""),  do: nil
  def get_cache(key) do
    unless Process.whereis(:ex_cache_ets), do: start
    LruCache.get(:ex_cache_ets, key)
  end


  @doc "Put a value in the cache"
  def put_cache(key, val) do
    unless Process.whereis(:ex_cache_ets), do: start
    LruCache.put(:ex_cache_ets, key, val)
    key
  end

  @doc "Remove all values from cache"
  def clear_cache do
    start
    :ets.delete_all_objects(:ex_cache_ets)
    save
  end
end
