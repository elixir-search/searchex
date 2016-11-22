defmodule Searchex.Build.Index.Render do

  @moduledoc false

  alias Searchex.Util.IO, as: DO

  def to_console do
    DO.puts "RENDER INDEX TO CONSOLE"
#    IO.puts "NUMKIDS:"
#    IO.inspect Supervisor.count_children(Searchex.KeywordSup)
  end
end
