defmodule Mix.Tasks.RunData do
  use Mix.Task

  @moduledoc false

  @shortdoc "Run Searchex CLI with full Dataset"
  def run(_) do
    Searchex.Cli.main ~w(build full)
  end
end
