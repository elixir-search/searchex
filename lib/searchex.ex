defmodule Searchex do

  @moduledoc "Application metadata"

  @doc "Return the application version"
  def version do
    {:ok, Searchex.Util.App.version}
  end
end
