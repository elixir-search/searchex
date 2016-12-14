defmodule Searchex.Command.Results do
  @moduledoc false

  def exec do
    Searchex.Command.Search.Cache.read_results
  end
end
