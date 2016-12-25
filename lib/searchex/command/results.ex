defmodule Searchex.Command.Results do

  @moduledoc false

  use Shake.Module
  alias Shake.Frame

  @doc "Module API"
  def exec(cfg_snip) do
    call(%Frame{cfg_snip: cfg_snip}, [])
  end

  step Searchex.Command.Index
  step :debug
  step :retrieve_query
  step :retrieve_results

  def debug(frame, _) do
    frame
  end

  def retrieve_query(%Frame{cfg_name: cfg_name} = frame, _opts) do
    query = Util.Cache.get_cache(frame, "#{cfg_name}_last_query")
    %Frame{frame | query: query}
  end

  def retrieve_results(%Frame{cfg_name: cfg_name} = frame, _opts) do
    results = Util.Cache.get_cache(frame, "#{cfg_name}_last_results")
    %Frame{frame | results: results}
  end
end
