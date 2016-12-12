defmodule Searchex.Command.Query do

  @moduledoc false

  use Shake.Module

  @doc "Module API"
  def exec(cfg_name, query) do
    call(%Frame{cfg_name: cfg_name, query: query}, [])
  end

  step Searchex.Command.Index
  step :do_query

  def do_query(%Frame{cfg_name: cfg_name, query: query} = frame, _opts) do
    results = {cfg_name, String.split(query)}
            |> Searchex.Keyword.Server.do_query
    %Frame{frame | results: results}
  end
end
