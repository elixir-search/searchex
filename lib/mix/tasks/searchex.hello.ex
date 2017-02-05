defmodule Mix.Tasks.Searchex.Hello do
  use Mix.Task

  @shortdoc "Example Task: HELLO!"
  def run(args) do
    IO.inspect args, label: "ARGS"
    IO.puts "SEARCHEX HELLO!"
  end
end
