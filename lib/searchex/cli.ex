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
    # Cmd      Arity    Module       Argument                 Description
    {"ls"      ,   0,   "Config"   , ""                     , "list collections"                       },
    {"info"    ,   0,   "Render"   , ""                     , "collection statistics"                  },
    {"fetch"   ,   1,   "Config"   , "GITHUB_REPO"          , "eg 'fetch elixir-search/sample'"        },
    {"new"     ,   1,   "Config"   , "TARGET_PATH"          , "new collection for TARGET_PATH"         },
    {"cat"     ,   1,   "Config"   , "COLLECTION"           , "cat config"                             },
    {"modify"  ,   1,   "Render"   , "COLLECTION"           , "edit the config file"                   },
    {"rm"      ,   1,   "Config"   , "COLLECTION"           , "remove config"                          },
    {"build"   ,   1,   "Render"   , "COLLECTION"           , "build the collection"                   },
    {"query"   ,   2,   "Render"   , "COLLECTION '<query>'" , "search the collection"                  },
    {"results" ,   1,   "Render"   , "COLLECTION"           , "results from the last query"            },
    {"show"    ,   2,   "Render"   , "COLLECTION ID"        , "show text of results ID"                },
    {"edit"    ,   2,   "Render"   , "COLLECTION ID"        , "edit results ID"                        },
    {"clean"   ,   1,   "Render"   , "COLLECTION"           , "remove cache for COLLECTION"            },
    {"version" ,   0,   ""         , ""                     , "show installed version"                 },
    {"help"    ,   0,   "Cli"      , ""                     , "this command"                           },
  ]
  @cmd_opts cmd_opts

  # These command options are not included in the CLI 'help' output.
  alt_opts = [
    # Cmd          Arity    Module       Argument             Description
    {"catalog"     ,   1,   "Render"   , "COLLECTION"       , "build the collection catalog"             },
    {"index"       ,   1,   "Render"   , "COLLECTION"       , "build the collection index"               },
    {"all_commands",   0,   "Cli"      , ""                 , "used for tab completion"                  },
    {"cfg_commands",   0,   "Cli"      , ""                 , "used for tab completion"                  },
    {"completion"  ,   0,   "Cli"      , ""                 , "renders the completion script"            },
  ]
  @alt_opts alt_opts

  # ----------------------------------------------------------------------------------------------------

  # Generate route functions for each command option
  for {cmd, len, mod, _args, _detail} <- cmd_opts ++ alt_opts do
    modfun = Util.Ext.Enum.join(["Searchex", mod, cmd], ".")
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

  def help do
    Searchex.Render.Help.to_table(@cmd_opts)
    :ok
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

  # Render command output to stdout
  defp render({_, str}) when is_binary(str), do: lcl_puts(str)
  defp render({_, lst}) when is_list(lst)  , do: lcl_test(lst)
  defp render({_, ele})                    , do: lcl_insp(ele)
  defp render(:ok)                         , do: :ok
  defp render({:ok})                       , do: :ok
  defp render(str) when is_binary(str)     , do: lcl_puts(str)
  defp render(ele)                         , do: lcl_insp(ele)

  defp lcl_test(list) do
    case Enum.all?(List.flatten(list), fn(el) -> is_binary(el) end) do
      true -> lcl_puts(Enum.join(list, "\n"))
      _    -> lcl_insp(list)
    end
  end

  defp lcl_puts(string) do
    unless Util.Ext.String.empty?(string), do: Util.Ext.IO.puts string
  end

  defp lcl_insp(ele) do
    Util.Ext.IO.inspect(ele, width: 999)
  end

  # ----------------------------------------------------------------------------------------------------

  def rpad(string, val), do: String.pad_trailing(string, val)
  def lpad(string, val), do: String.pad_leading(string, val)

  defp prog         , do: Util.Ext.App.name

#  defp prog_version , do: Util.Ext.App.version

  defp usage_message, do: "Type '#{prog} help' for usage information."

  defp error(argv) do
    str = """
    ERROR unrecognized command (#{prog} #{Enum.join(argv, " ")})
    #{usage_message}
    """
    {:error, str}
  end
end

