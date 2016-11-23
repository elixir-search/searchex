defmodule Searchex.Cfg do

  @moduledoc """
  Manage Searchex configurations.
  
  A configuration is stored in a yaml file under `~/.searchex`.
  """

  alias Searchex.Util.IO, as: DIO

  @base_cfg_dir "~/.searchex"
  @test_cfg_dir "test/data/configs"

  @default_cfg File.read("eex/default_cfg.yml.eex")

  @doc "Create a new config"
  # TODIO: CHANGE TO PATH INDEXING...
  def new(cfg_name) do
    make_cfg_dir()
    cond do
      cfg_name_invalid?(cfg_name) -> {:error, cfg_name_invalid_msg(cfg_name)}
      cfg_exists?(cfg_name)       -> {:error, cfg_exists_msg(cfg_name)}
      true                        -> create_cfg(cfg_name)
    end
  end

  @doc "Fetch a config from elixir-search/sample_docs"
  def fetch() do
    DIO.puts "FETCH : UNDER CONSTRUCTION"
  end

  @doc "Return the contents of a config"
  def cat(cfg_name) do
    cond do
      cfg_name_invalid?(cfg_name) -> {:error, cfg_name_invalid_msg(cfg_name)}
      cfg_missing?(cfg_name)      -> {:error, cfg_missing_msg(cfg_name)}
      true                        -> cfg_contents(cfg_name)
    end
  end

  @doc """
  Launch an editor to update a config.  NOTE: you must define environment
  variables `EDITOR` and `TERMINAL`.  This will not work over SSH.

  This needs fixing.  See this thread for more info:
  https://elixirforum.com/t/how-to-launch-an-editor-from-escript/2094/1
  """
  def edit(cfg_name) do
    make_cfg_dir()
    cond do
      missing_editor?()           -> {:error, missing_editor_msg()}
      missing_terminal?()         -> {:error, missing_terminal_msg()}
      connected_via_ssh?()        -> {:error, connected_via_ssh_msg(cfg_name)}
      cfg_name_invalid?(cfg_name) -> {:error, cfg_name_invalid_msg(cfg_name)}
      cfg_missing?(cfg_name)      -> {:error, cfg_missing_msg(cfg_name)}
      true                        -> edit_cfg(cfg_name)
    end
  end

  @doc "Remove a config"
  def rm(cfg_name) do
    cond do
      cfg_name_invalid?(cfg_name) -> {:error, cfg_name_invalid_msg(cfg_name)}
      cfg_missing?(cfg_name)      -> {:error, cfg_missing_msg(cfg_name)}
      true                        -> System.cmd("rm", [cfg_file(cfg_name)]); {:ok}
    end
  end

  @doc "List the configs"
  def ls do
    make_cfg_dir()
    {files, _code} = System.cmd("ls", [], cd: cfg_dir())
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
    {:ok, yaml_data} = cat(cfg_name)
    YamlElixir.read_from_string yaml_data
  end

  # ---------------------------------------------------------------

  # Use separate configs for tests...
  defp cfg_dir do
    case Mix.env do
      :test -> @test_cfg_dir
      _     -> @base_cfg_dir
    end
    |> Path.expand
  end

  defp cfg_file(cfg_name) do
    cfg_dir() <> "/" <> cfg_name <> ".yml"
  end

  defp make_cfg_dir(), do: System.cmd("mkdir", ["-p", cfg_dir()])

  # -----

  defp cfg_contents(cfg_name) do
    {:ok, str} = File.read(cfg_file(cfg_name))
    {:ok, str}
  end

  defp create_cfg(cfg_name) do
    {:ok, text} = @default_cfg
    File.write(cfg_file(cfg_name), text)
    {:ok}
  end

  defp edit_cfg(cfg_name) do
    System.cmd(terminal(), ["-x", editor(), cfg_file(cfg_name)])
    {:ok}
  end

  # -----

  defp cfg_exists_msg(cfg_name), do:
  "Config already exists (#{cfg_name})"

  defp cfg_missing_msg(cfg_name), do:
  "Config not found (#{cfg_name})"

  defp cfg_name_invalid_msg(cfg_name), do:
  "Invalid config name (#{cfg_name})"

  defp connected_via_ssh_msg(cfg_name), do:
  "Can't launch editor over SSH.  Run `#{editor()} #{cfg_file(cfg_name)}`"

  defp missing_editor_msg(), do:
  "No EDITOR defined - put `export EDITOR=<editor>` in your `.bashrc`"

  defp missing_terminal_msg(), do:
  "No TERMINAL defined - put `export TERMINAL=<terminal>` in your `.bashrc`"

  # -----

  defp editor()  , do: System.get_env("EDITOR")

  defp terminal(), do: System.get_env("TERMINAL")

  # -----

  defp cfg_name_valid?(cfg_name)  , do: ! Regex.match?(~r/[^0-9A-Za-z-_]/, cfg_name) # no punct or ws

  defp cfg_name_invalid?(cfg_name), do: ! cfg_name_valid?(cfg_name)

  defp cfg_exists?(cfg_name)      , do: File.exists?(cfg_file(cfg_name))

  defp cfg_missing?(cfg_name)     , do: ! cfg_exists?(cfg_name)

  defp connected_via_ssh?()       , do: System.get_env("SSH_CLIENT") != nil

  defp has_editor?()              , do: editor() != nil

  defp missing_editor?()          , do: ! has_editor?()

  defp has_terminal?()            , do: terminal() != nil

  defp missing_terminal?()        , do: ! has_terminal?()

end
