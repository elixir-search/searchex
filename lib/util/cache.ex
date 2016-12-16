defmodule Util.Cache do
  @moduledoc false
  # Simple LRU cache, with optional persistence to disk.

  @doc """
  Start the cache service for a collection
  """
  def start(cfg_name, size \\ 50) do
    {ets_name, dets_name, dets_path} = expand(cfg_name)
    unless Process.whereis(ets_name) do
      LruCache.start_link(ets_name, size)
      :dets.open_file(dets_name, [file: dets_path])
      :ets.from_dets(ets_name, dets_name)
      :dets.close(dets_name)
    end
    :ok
  end

  @doc "Stop the cache process"
  def stop(cfg_name) do
    save(cfg_name)
    {ets_name, dets_name, _} = expand(cfg_name)
    if Process.whereis(ets_name),  do: GenServer.stop(ets_name)
    if Process.whereis(dets_name), do: GenServer.stop(dets_name)
  end

  @doc "Save the cache data to disk"
  def save(cfg_name) do
    {ets_name, dets_name, dets_path} = expand(cfg_name)
    :dets.open_file(dets_name, [file: dets_path])
    :ets.to_dets(ets_name, dets_name)
    :dets.close(dets_name)
  end

  @doc "Get a value from the cache"
  def get_cache(_cfg_name, ""),  do: nil
  def get_cache(cfg_name, key) do
    {ets_name, _, _} = expand(cfg_name)
    start(cfg_name)
    LruCache.get(ets_name, Util.Ext.Term.to_atom(key))
  end

  @doc "Put a value in the cache"
  def put_cache(cfg_name, key, val) do
    start(cfg_name)
    {ets_name, _, _} = expand(cfg_name)
    LruCache.put(ets_name, Util.Ext.Term.to_atom(key), val)
    key
  end

  @doc "Remove all values from cache"
  def clear_cache(cfg_name) do
    start(cfg_name)
    {ets_name, _, _} = expand(cfg_name)
    :ets.delete_all_objects(ets_name)
    save(cfg_name)
  end

  defp expand(cfg_name) do
    cfg_str   = to_string(cfg_name)
    ets_name  = Util.Ext.Term.to_atom("ets_#{cfg_str}")
    dets_name = Util.Ext.Term.to_atom("dets_#{cfg_str}")
    dets_path = Searchex.settings[:data] <> "/#{cfg_str}.dets"
                |> Path.expand
                |> String.to_charlist
    {ets_name, dets_name, dets_path}
  end

end
