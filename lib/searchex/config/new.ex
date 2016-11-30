defmodule Searchex.Config.New do
  @moduledoc false

  import Searchex.Config.Helpers

  @default_cfg File.read("eex/default_cfg.yml.eex")

  def exec(path) do
      full_path = Path.expand(path)
      cfg_name = name_from_path(path)
      make_active_dirs()
      cond do
        cfg_dir_absent?(full_path)  -> {:error, cfg_dir_missing_msg(path)}
        cfg_exists?(cfg_name)       -> {:error, cfg_exists_msg(cfg_name)}
        true                        -> create_cfg(cfg_name, full_path)
      end
  end

  # -----

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
      max_numdocs:     1000             ,
      max_doc_kb:      100              ,
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