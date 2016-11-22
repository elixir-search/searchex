defmodule Mix.Tasks.RunHello do
  use Mix.Task

  @moduledoc false

  @shortdoc "HELLO"
  def run(_) do
    Mix.shell.info "HELLO THERE"
  end
end
