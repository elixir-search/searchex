defmodule Searchex.Cfg do

  @moduledoc """
  Manage Searchex configurations.
  
  A configuration is stored in a yaml file under `~/.searchex/cfgs`.
  """

  @base_cfg_dir "~/.searchex/cfgs"
  @test_cfg_dir "test/data/configs"

  @default_cfg File.read("eex/default_cfg.yml.eex")

  @doc "Create a new config"
  def cfg_new(path) do
    full_path = Path.expand(path)
    cfg_name  = String.split(full_path, "/") |> List.last
    make_active_dirs()
    cond do
      cfg_dir_absent?(full_path)  -> {:error, cfg_dir_missing_msg(path)}
      cfg_exists?(cfg_name)       -> {:error, cfg_exists_msg(cfg_name)}
      true                        -> create_cfg(cfg_name, full_path)
    end
  end

  @doc "Fetch a config from elixir-search/sample_docs"
  def cfg_fetch(_tmp) do
    DIO.puts "FETCH : UNDER CONSTRUCTION"
    {:error, "Pending Implementation"}
  end

  @doc "Return the contents of a config"
  def cfg_cat(cfg_name) do
    cond do
      cfg_name_invalid?(cfg_name) -> {:error, cfg_name_invalid_msg(cfg_name)}
      cfg_missing?(cfg_name)      -> {:error, cfg_missing_msg(cfg_name)}
      true                        -> cfg_contents(cfg_name)
    end
  end

  @doc """
  Launch an editor to update a config.  NOTE: you must define environment
  variables `EDITOR`.  This will only work with TMUX.

  This needs fixing.  See this thread for more info:
  https://elixirforum.com/t/how-to-launch-an-editor-from-escript/2094/1
  """
  def cfg_edit(cfg_name) do
    make_active_dirs()
    cond do
      missing_editor?()           -> {:error, missing_editor_msg()}
      connected_without_tmux?()   -> {:error, connected_without_tmux_msg(cfg_name)}
      cfg_name_invalid?(cfg_name) -> {:error, cfg_name_invalid_msg(cfg_name)}
      cfg_missing?(cfg_name)      -> {:error, cfg_missing_msg(cfg_name)}
      true                        -> edit_cfg(cfg_name)
    end
  end

  @doc "Remove a config"
  def cfg_rm(cfg_name) do
    cond do
      cfg_name_invalid?(cfg_name) -> {:error, cfg_name_invalid_msg(cfg_name)}
      cfg_missing?(cfg_name)      -> {:error, cfg_missing_msg(cfg_name)}
      true                        -> System.cmd("rm", [cfg_file(cfg_name)]); {:ok}
    end
  end

  @doc "List the configs"
  def cfg_ls do
    make_active_dirs()
    {files, _code} = System.cmd("ls", [], cd: active_dirs.cfgs)
    output = files
             |> String.split("\n")
             |> Enum.filter(&(Regex.match?(~r/yml$/, &1)))
             |> Enum.map(&(String.replace(&1, ~r/.yml$/, "")))
    {:ok, output}
  end

  @doc "Return the CFG data as a Map"
  def to_map({:ok, yaml_data}) do
    YamlElixir.read_from_string yaml_data, atoms: true
  end
  def to_map(cfg_name) do
    {:ok, yaml_data} = cfg_cat(cfg_name)
    YamlElixir.read_from_string yaml_data
  end

  # ---------------------------------------------------------------

  defp base_dirs do
    %{
      cfgs: "~/.searchex/cfgs"   ,
      docs: "~/.searchex/docs"   ,
      data: "~/.searchex/data"   ,
      temp: "~/.searchex/temp"
    }
  end

  defp test_dirs do
    %{
      cfgs: "test/data/configs"   ,
      docs: "/tmp/searchex_test/docs"    ,
      data: "/tmp/searchex_test/data"    ,
      temp: "/tmp/searchex_test/temp"
    }
  end

  defp active_dirs do
    case Mix.env do
      :test -> test_dirs
      _     -> base_dirs
    end
    |> Enum.reduce(%{}, fn({k,v}, acc) -> Map.merge(acc, %{k => Path.expand(v)}) end )
  end

  defp cfg_file(cfg_name) do
    active_dirs.cfgs <> "/" <> cfg_name <> ".yml"
  end

  defp make_active_dirs() do
    System.cmd("mkdir", ["-p"] ++ Map.values(active_dirs))
  end

  # -----

  defp cfg_contents(cfg_name) do
    {:ok, str} = File.read(cfg_file(cfg_name))
    {:ok, str}
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

  defp edit_cfg(cfg_name) do
    System.cmd "tmux", ["split-window", "vim", cfg_file(cfg_name)]
    {:ok}
  end

  # -----

  defp cfg_dir_missing_msg(path), do:
  "Path does not exist (#{path})"

  defp cfg_exists_msg(cfg_name), do:
  "Config already exists (#{cfg_name})"

  defp cfg_missing_msg(cfg_name), do:
  "Config not found (#{cfg_name})"

  defp cfg_name_invalid_msg(cfg_name), do:
  "Invalid config name (#{cfg_name})"

  defp connected_without_tmux_msg(cfg_name), do:
  "Can't launch editor without TMUX.  Run `#{editor()} #{cfg_file(cfg_name)}`"

  defp missing_editor_msg(), do:
  "No EDITOR defined - put `export EDITOR=<editor>` in your `.bashrc`"

  # -----

  defp editor()  , do: System.get_env("EDITOR")

  # -----

  defp cfg_dir_exists?(path)      , do: File.dir?(path)

  defp cfg_dir_absent?(path)      , do: ! cfg_dir_exists?(path)

  defp cfg_name_valid?(cfg_name)  , do: ! Regex.match?(~r/[^0-9A-Za-z-_]/, cfg_name) # no punct or ws

  defp cfg_name_invalid?(cfg_name), do: ! cfg_name_valid?(cfg_name)

  defp cfg_exists?(cfg_name)      , do: File.exists?(cfg_file(cfg_name))

  defp cfg_missing?(cfg_name)     , do: ! cfg_exists?(cfg_name)

  defp connected_using_tmux?()    , do: System.get_env("TMUX") != nil

  defp connected_without_tmux?()  , do: ! connected_using_tmux?

  defp has_editor?()              , do: editor() != nil

  defp missing_editor?()          , do: ! has_editor?()

end
