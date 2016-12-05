defmodule ExCache do
  @moduledoc """
  Simple LRU cache, with optional persistence to disk.
  """

  @doc "Start the cache service"
  def start(opts \\ []) do
    size = opts[:size] || 50
    path = opts[:path] || nil
    LruCache.start_link(:ex_cache_ets, size)
    if path do
      :dets.open_file(:ex_cache_dets, [file: path])
      :ets.from_dets(:ex_cache_ets, :ex_cache_dets)
      :dets.close(:ex_cache_dets)
    end
    GlobalState.set(:ex_cache_path, path)
  end

  @doc "Save the cache data to disk"
  def save do
    if path = GlobalState.get(:ex_cache_path) do
      :dets.open_file(:ex_cache_dets, [file: path])
      :ets.to_dets(:ex_cache_ets, :ex_cache_dets)
      :dets.close(:ex_cache_dets)
    end
  end

  @doc "Get a value from the cache"
  def get_cache(key)     , do: LruCache.get(:ex_cache_ets, key)

  @doc "Put a value in the cache"
  def put_cache(key, val), do: LruCache.put(:ex_cache_ets, key, val)

  @doc "Remove all values from cache"
  def clear_cache do
    :ets.delete_all_objects(:ex_cache_ets)
    save
  end
end
