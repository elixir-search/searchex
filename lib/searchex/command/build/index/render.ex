defmodule Searchex.Command.Build.Index.Render do

  @moduledoc false

  def to_console do
    Util.Ext.IO.puts "RENDER INDEX TO CONSOLE"
#    IO.puts "NUMKIDS:"
#    IO.inspect Supervisor.count_children(Searchex.Keyword.Supervisor)
  end
end
