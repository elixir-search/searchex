defmodule Mix.Tasks.RunTest do
  use Mix.Task

  @moduledoc false

  @shortdoc "Run Searchex CLI with test Dataset"
  def run(_) do
    Searchex.Cli.main ~w(build test)
  end
end
