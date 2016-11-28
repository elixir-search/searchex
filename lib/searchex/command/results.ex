defmodule Searchex.Command.Results do
  use ExMake

  # error checks:
  # - valid cfg_name
  # - existing cfg_name
  # - valid cfg
  def exec do
    Searchex.Command.Search.Cache.read_results
    |> Searchex.Command.Search.Results.render
  end

  def handle_chain(_cfg_name) do
    "TBD"
  end
end