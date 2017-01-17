defmodule Util.Cache do
  @moduledoc false
  # Simple LRU cache, with optional persistence to disk.

  @doc """
  Start the cache service for a collection
  """
  def start(frame, size \\ 50) do
    {ets_name, dets_name, dets_path} = expand(frame)
    unless Process.whereis(ets_name) do
      alias Searchex.Request.Util.Helpers
      File.mkdir_p(Helpers.cache_dir(frame))
      LruCache.start_link(ets_name, size)
      :dets.open_file(dets_name, [file: dets_path])
      :ets.from_dets(ets_name, dets_name)
      :dets.close(dets_name)
    end
    :ok
  end

  @doc "Stop the cache process"
  def stop(frame) do
    save(frame)
    {ets_name, dets_name, _} = expand(frame)
    if Process.whereis(ets_name),  do: GenServer.stop(ets_name)
    if Process.whereis(dets_name), do: GenServer.stop(dets_name)
  end

  @doc "Save the cache data to disk"
  def save(frame) do
    unless frame.halted do
      {ets_name, dets_name, dets_path} = expand(frame)
      if Process.whereis(ets_name) do
        :dets.open_file(dets_name, [file: dets_path])
        :ets.to_dets(ets_name, dets_name)
        :dets.close(dets_name)
      end
    end
    frame
  end

  def save(frame, _el) do
    save(frame)
    frame
  end

  @doc "Get a value from the cache"
  def get_cache(_frame, ""),  do: nil
  def get_cache(frame, key) do
    {ets_name, _, _} = expand(frame)
    start(frame)
    LruCache.get(ets_name, Util.Ext.Term.to_atom(key))
  end

  @doc "Put a value in the cache"
  def put_cache(frame, key, val) do
    start(frame)
    {ets_name, _, _} = expand(frame)
    LruCache.put(ets_name, Util.Ext.Term.to_atom(key), val)
    key
  end

  @doc "Remove all values from cache"
  def clear_cache(frame) do
    start(frame)
    {ets_name, _, _} = expand(frame)
    :ets.delete_all_objects(ets_name)
    save(frame)
  end

  defp expand(frame) do
    alias Searchex.Request.Util.Helpers
    cfg_str   = to_string(frame.cfg_name) |> String.replace("/", "_")
    ets_name  = Util.Ext.Term.to_atom("ets_#{cfg_str}")
    dets_name = Util.Ext.Term.to_atom("dets_#{cfg_str}")
    dets_path = Helpers.cache_file(frame) |> String.to_charlist
    {ets_name, dets_name, dets_path}
  end
end
