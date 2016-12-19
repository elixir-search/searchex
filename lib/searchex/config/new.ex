defmodule Searchex.Config.New do
  @moduledoc false

  import Searchex.Config.HelpersOld
  import Util.CfgValidations

  @default_cfg File.read("eex/default_cfg.yml.eex")

  def exec(path) do
      full_path = Path.expand(path)
      cfg_name = name_from_path(path)
      make_active_dirs()
      case check_validations(validation_list(cfg_name)) do
        {:error, msgs} -> {:error, msgs}
        {:ok}          -> create_cfg(cfg_name, full_path)
      end
  end

  # -----

  defp validation_list(cfg_name) do
    [
      cfg_name_invalid?(cfg_name) ,
      cfg_present?(cfg_name)
    ]
  end

  defp create_cfg(cfg_name, root_path) do
    File.write(cfg_file(cfg_name), default_cfg_text(cfg_name, root_path))
    {:ok}
  end

  defp default_cfg_text(cfg_name, docroot) do
    {:ok, text} = @default_cfg
    EEx.eval_string(text, cfg_options(docroot: docroot, cfg_name: cfg_name))
  end

  defp cfg_options(new_opts) do
    types = ~w(txt md json xml js rb java ex exs cfg toml)
    default_opts = [
      cfg_name:        "TBD"            ,   # the name of the config (collection?)
      file_types:      quotify(types)   ,   # file types
      docroot:         Path.expand("~") ,
      max_numfiles:    500              ,
      max_file_kb:     500              ,
      docsep:          "NNNN"           ,
      cli_style:       "TBD"            ,
    ]
    Keyword.merge(default_opts, new_opts)
  end

    defp quotify(array_of_string) do
      array_of_string
      |> Enum.map(&("\"#{&1}\""))
      |> Enum.join(", ")
    end
end