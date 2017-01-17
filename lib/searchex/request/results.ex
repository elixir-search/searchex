defmodule Searchex.Request.Results do

  @moduledoc false

  use Shreq.Module
  alias Shreq.Frame

  @doc "Module API"
  def exec(cfg_snip) do
    call(%Frame{cfg_snip: cfg_snip}, [])
  end

  step Searchex.Request.Index
  step :retrieve_query
  step :retrieve_results

  def retrieve_query(%Frame{cfg_name: cfg_name} = frame, _opts) do
    query = Util.Cache.get_cache(frame, "#{cfg_name}_last_query")
    %Frame{frame | query: query}
  end

  def retrieve_results(%Frame{cfg_name: cfg_name} = frame, _opts) do
    results = Util.Cache.get_cache(frame, "#{cfg_name}_last_results")
    %Frame{frame | results: results}
  end
end
