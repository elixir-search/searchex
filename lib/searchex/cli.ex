defmodule Searchex.Cli do

  @moduledoc false
 
  @completion_script File.read("eex/searchex_completion.bash")

  @doc """
  Entry point for the `searchex` executable.  Takes a single argument `argv` which
  is a list of command-line options.
  """
  def main(argv) do
    route(argv) 
    |> render
  end
  
  # List of command options.  The command should be the same as the function
  # name.  Argument and Description are used to generate help text.
  cmd_opts = [
    # Cmd      Arity  Module   Argument                   Description
    {"new"     ,   1,   "Cfg"  , "COLLECTION"           , "create new config"       },
    {"cat"     ,   1,   "Cfg"  , "COLLECTION"           , "cat config"              },
    {"edit"    ,   1,   "Cfg"  , "COLLECTION"           , "edit config"             },
    {"test"    ,   1,   "Cfg"  , "COLLECTION"           , "test config"             },
    {"rm"      ,   1,   "Cfg"  , "COLLECTION"           , "remove config"           },
    {"ls"      ,   0,   "Cfg"  , ""                     , "list configs"            },
    {"catalog" ,   1,   "Cmd"  , "COLLECTION"           , "catalog the collection"  },
    {"index"   ,   1,   "Cmd"  , "COLLECTION"           , "index the collection"    },
    {"build"   ,   1,   "Cmd"  , "COLLECTION"           , "catalog and index"       },
    {"search"  ,   2,   "Cmd"  , "COLLECTION '<query>'" , "search the collection"   },
    {"version" ,   0,   "Cli"  , ""                     , "show installed version"  },
    {"help"    ,   0,   "Cli"  , ""                     , "this command"            },
  ]
  @cmd_opts cmd_opts

  @doc """
  Route command-line input to the appropriate command.
  """

  # generate functions for each cmd_opt
  for {cmd, len, mod, _args, _detail} <- cmd_opts do
    {efunc, _} = Code.eval_string("&Searchex.#{mod}.#{cmd}/#{len}")
    @cmd  cmd
    @func efunc
    case len do
      0 -> def route([@cmd])               , do: @func.()
      1 -> def route([@cmd, col_name])     , do: @func.(col_name) 
      2 -> def route([@cmd, col_name, qry]), do: @func.(col_name, qry)
      _ -> raise("ERROR Bad Option Data (#{cmd}/#{len})")
    end
  end

  def route(["all_commands"]), do: all_commands()   # used for tab-completion...
  def route(["cfg_commands"]), do: cfg_commands()   # used for tab-completion...
  def route(["completion"])  , do: completion()     # used for tab-completion...

  def route([])  , do: help 
  def route(argv), do: error(argv)

  @doc "Generate a help message"
  def help() do
    tfunc = fn({cmd, _len, _mod, args, detail}) -> {"#{prog} #{cmd} #{args}", "# #{detail}"} end
    tmp   = Enum.map @cmd_opts, tfunc
    maxln = Enum.reduce tmp, 0, fn({first, _last}, acc) -> max(String.length(first), acc) end
    lines = Enum.map tmp, fn({first, last}) -> "  #{String.pad_trailing(first, maxln)} #{last}" end
    value = ["""

    #{String.upcase(prog)} VERSION #{prog_version} - NOT READY FOR USE
    #{Enum.join(lines, "\n")}

    Visit https://github.com/andyl/searchex for more info...
    """]
    {:ok, value}
  end

  @doc "Write the installed version to stdout"
  def version, do: {:ok, [prog_version()]}

  @doc "Show the tab-completion script"
  def completion do
    @completion_script
  end

  # -------------------------------------------------------------------

  # List all commands - used for tab-completion
  defp all_commands do
    cmds = @cmd_opts
    |> Enum.map(fn({cmd, _arity, _module, _arg, _desc}) -> cmd end)
    |> Enum.join("\n")
    {:ok, cmds}
  end

  # List commands that take the name of an existing config - used for tab-completion
  defp cfg_commands do
    cmds = @cmd_opts
    |> Enum.filter(fn({cmd, arity, _module, _arg, _desc}) -> cmd != "new" && arity > 0 end)
    |> Enum.map(fn({cmd, _arity, _module, _arg, _desc}) -> cmd end)
    |> Enum.join("\n")
    {:ok, cmds}
  end

  # -----

  # Render command output to stdout
  defp render({:ok})       , do: {:ok}
  defp render({:ok   , []}), do: {:ok}
  defp render({:ok   , str}) when is_binary(str), do: lcl_puts str
  defp render({:error, str}) when is_binary(str), do: lcl_puts str
  defp render({:ok   , list}), do: lcl_puts Enum.join(list, "\n")
  defp render({:error, list}), do: lcl_puts Enum.join(list, "\n")
  defp render(_val), do: lcl_puts "ERROR: RENDER FAILURE"

  defp lcl_puts(string) do
    Searchex.Util.IO.puts string
  end
  
  # -----

  defp prog         , do: Searchex.Util.App.name

  defp prog_version , do: Searchex.Util.App.version

  defp usage_message, do: "Type '#{prog} help' for usage information."

  defp error(argv) do
    str = """
    ERROR unrecognized command (#{prog} #{Enum.join(argv, " ")})"
    #{usage_message}
    """
    {:error, str}
  end
end

