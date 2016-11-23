defmodule Searchex.Build.Index.Render do

  @moduledoc false

  alias Searchex.Util.IO, as: DIO

  def to_console do
    DIO.puts "RENDER INDEX TO CONSOLE"
#    IO.puts "NUMKIDS:"
#    IO.inspect Supervisor.count_children(Searchex.KeywordSup)
  end
end
