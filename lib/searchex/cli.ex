defmodule Searchex.Cli do

  @moduledoc false
 
  @completion_script File.read("eex/searchex_completion.bash")

  @doc """
  Entry point for the `searchex` executable.  Takes a single argument `argv` which
  is a list of command-line options.
  """
  def main(argv) do
    route(argv) |> render
  end

  # List of command options.  The command should be the same as the function
  # name.  Argument and Description are used to generate help text.
  cmd_opts = [
    # Cmd        Arity     Module       Argument                 Description
    {"cfg_new"   ,   1,   "Config"   , "TARGET_PATH"          , "new config for TARGET_PATH"           },
    {"cfg_cat"   ,   1,   "Config"   , "COLLECTION"           , "cat config"                           },
    {"cfg_edit"  ,   1,   "Render"   , "COLLECTION"           , "edit config"                          },
    {"cfg_rm"    ,   1,   "Config"   , "COLLECTION"           , "remove config"                        },
    {"cfg_ls"    ,   0,   "Config"   , ""                     , "list configs"                         },
    {"build"     ,   1,   "Command"  , "COLLECTION"           , "build the collection"                 },
    {"search"    ,   2,   "Render"   , "COLLECTION '<query>'" , "search the collection"                },
    {"results"   ,   0,   "Render"   , ""                     , "show results from the last search"    },
    {"show"      ,   1,   "Command"  , "ID"                   , "show text of ID"                      },
    {"edit"      ,   1,   "Render"   , "ID"                   , "edit ID"                              },
    {"version"   ,   0,   ""         , ""                     , "show installed version"               },
    {"help"      ,   0,   "Cli"      , ""                     , "this command"                         },
  ]
  @cmd_opts cmd_opts

  # These command options are not included in the CLI 'help' output.
  alt_opts = [
    # Cmd          Arity      Module       Argument               Description
    {"query"       ,   2,   "Render"   , "COLLECTION '<query>'", "alias for search"                         },
    {"cfg_fetch"   ,   1,   "Config"   , "SAMPLE"              , "fetch from elixir-search/sample_docs"     },
    {"catalog"     ,   1,   "Command"  , "COLLECTION"          , "catalog the collection"                   },
    {"index"       ,   1,   "Command"  , "COLLECTION"          , "index the collection"                     },
    {"info"        ,   1,   "Command"  , "COLLECTION"          , "show collection status and statistics"    },
    {"clean"       ,   0,   "Cli"      , ""                    , "remove all cached assets"                 },
    {"all_commands",   0,   "Cli"      , ""                    , "used for tab completion - lists all cmds" },
    {"cfg_commands",   0,   "Cli"      , ""                    , "used for tab completion"                  },
    {"completion"  ,   0,   "Cli"      , ""                    , "renders the completion script"            },
  ]
  @alt_opts alt_opts

  # ----------------------------------------------------------------------------------------------------

  # Generate route functions for each command option
  for {cmd, len, mod, _args, _detail} <- cmd_opts ++ alt_opts do
    modfun = Searchex.Util.Enum.join(["Searchex", mod, cmd], ".")
    {efunc, _} = Code.eval_string("&#{modfun}/#{len}")
    @cmd  cmd
    @func efunc
    case len do
      0 -> def route([@cmd])               , do: @func.()
      1 -> def route([@cmd, col_name])     , do: @func.(col_name)
      2 -> def route([@cmd, col_name, qry]), do: @func.(col_name, qry)
      _ -> raise("ERROR Bad Option Data (#{cmd}/#{len})")
    end
  end

  def route([])  , do: help
  def route(argv), do: error(argv)

  # ----------------------------------------------------------------------------------------------------

  # Generate a help message
  def help do
    lines = 
      Enum.map(@cmd_opts, fn({cmd,_,_,args,detail}) -> 
        [rpad(cmd,8), rpad("|",1), rpad(args,20), "|", detail <> "\n" ]
        |> Enum.join(" ") 
      end)
    headers = [rpad("Command", 11), rpad("Arguments", 23), "Description"]
    value = [String.upcase(prog) <> " Version #{prog_version} - NOT READY FOR USE\n",headers, lines]
    {:ok, value}
  end

  # Show the tab-completion script
  def completion do
    @completion_script
  end

  # List all commands - used for tab-completion
  def all_commands do
    cmds = @cmd_opts
    |> Enum.map(fn({cmd, _arity, _module, _arg, _desc}) -> cmd end)
    |> Enum.join("\n")
    {:ok, cmds}
  end

  # List commands that take the name of an existing config - used for tab-completion
  def cfg_commands do
    cmds = @cmd_opts
    |> Enum.filter(fn({cmd, arity, _module, _arg, _desc}) -> cmd != "new" && arity > 0 end)
    |> Enum.map(fn({cmd, _arity, _module, _arg, _desc}) -> cmd end)
    |> Enum.join("\n")
    {:ok, cmds}
  end

  # For testing...
  def command_list_for_testing do
    @cmd_opts ++ @alt_opts
    |> Enum.map(&(elem(&1,0)))
  end

  # ----------------------------------------------------------------------------------------------------

  def clean do
    Searchex.Config.Helpers.clean
    {:ok, ""}
  end

  # ----------------------------------------------------------------------------------------------------

  # Render command output to stdout
  defp render({:ok   , str}) when is_binary(str), do: lcl_puts str
  defp render({:error, str}) when is_binary(str), do: lcl_puts str
  defp render({:ok   , list}), do: lcl_puts Enum.join(list, "\n")
  defp render({:error, list}), do: lcl_puts Enum.join(list, "\n")
  defp render(_val), do: {:ok}

  defp lcl_puts(string) do
    unless Searchex.Util.String.empty?(string), do: DIO.puts string
  end

  # ----------------------------------------------------------------------------------------------------

  def rpad(string, val), do: String.pad_trailing(string, val)
  def lpad(string, val), do: String.pad_leading(string, val)

  defp prog         , do: Searchex.Util.App.name

  defp prog_version , do: Searchex.Util.App.version

  defp usage_message, do: "Type '#{prog} help' for usage information."

  defp error(argv) do
    str = """
    ERROR unrecognized command (#{prog} #{Enum.join(argv, " ")})
    #{usage_message}
    """
    {:error, str}
  end
end

