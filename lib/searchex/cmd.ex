defmodule Searchex.Cmd do
  @moduledoc """
  Main Searchex workflow

  1. Read Configuration
  2. Build Catalog
  3. Build Index
  4. Perform Query

  This workflow establishes a dependency chain, with higher level steps
  depending on the outputs of lower level steps.  Each step generates an intermediate
  output which can be cached to minimize re-execution of compute-intensive steps.

  - Read Configuration generates in-memory state
  - Build Catalog generates an on-disk cache file (~/.searchex/data/<collection>_cat.dat)
  - Build Index generates an on-disk cache file (~/.searchex/data/<collection>_idx.dat)
  - Perform Query generates a results file (~/.searchex/temp/results.dat)

  The overall dependency tree starts with on-disk assets:

  [config file | document directories | executable compile date ] < Read Configuration
  < Build Index < Perform Query

  The `Cmd` system uses the Elixir behavior `ExMake` to manage the dependency chain.
  """

  def catalog(cfg_name) do
    DIO.puts "CATALOG #{cfg_name}"
    Searchex.Build.Catalog.assemble(cfg_name)
  end

  def index(cfg_name) do
    DIO.puts "INDEX #{cfg_name}"
    catalog = catalog(cfg_name)
    Searchex.Build.Index.read_or_generate(catalog)
    catalog
  end

  def build(cfg_name) do
    DIO.puts "BUILD #{cfg_name}"
    index(cfg_name)
  end

  def info(_cfg_name) do
    {:ok, "INFO: UNDER CONSTRUCTION"}
  end

  def search(cfg_name, query) do
    DIO.puts "SEARCH #{cfg_name} #{query}"
    {index(cfg_name), query}
    |> Searchex.Search.do_query
    |> Searchex.Search.Results.filter
    |> Searchex.Search.Cache.write_results
    |> Searchex.Search.Results.render
  end

  def query(cfg_name, query) do
    search(cfg_name, query)
  end

  def results do
    Searchex.Search.Cache.read_results
    |> Searchex.Search.Results.render
  end

  def show(idnum) do
    results = elem(Searchex.Search.Cache.read_results,0)
    docs    = results.docs
    doc     = Enum.at(docs, String.to_integer(idnum))
    body = doc.body
    DIO.puts body
    {:ok}
  end

    def edit(idnum) do
        results = elem(Searchex.Search.Cache.read_results,0)
        docs    = results.docs
        doc     = Enum.at(docs, String.to_integer(idnum))
        DIO.inspect doc, color: "green"
      cond do
        missing_editor?()           -> {:error, missing_editor_msg()}
        connected_without_tmux?()   -> {:error, connected_without_tmux_msg(doc)}
        true                        -> edit_doc(doc)
      end
    end

    defp edit_doc(doc_params) do
      System.cmd "tmux", ["split-window", "vim", doc_file(doc_params)]
      {:ok}
    end

    # -----

#    defp cfg_dir_missing_msg(path), do:
#    "Path does not exist (#{path})"

#    defp cfg_exists_msg(cfg_name), do:
#    "Config already exists (#{cfg_name})"

#    defp cfg_missing_msg(cfg_name), do:
#    "Config not found (#{cfg_name})"

#    defp cfg_name_invalid_msg(cfg_name), do:
#    "Invalid config name (#{cfg_name})"

    defp connected_without_tmux_msg(doc_params), do:
    "Can't launch editor without TMUX.  Run `#{editor()} #{doc_file(doc_params)}`"

    defp missing_editor_msg(), do:
    "No EDITOR defined - put `export EDITOR=<editor>` in your `.bashrc`"

    # -----



  defp doc_file(doc_params) do
    basename = doc_params.filename
    extname  = Path.expand(basename)
    docstart = doc_params.docstart
#    active_dirs.cfgs <> "/" <> cfg_name <> ".yml"
    # TODO: make the editor start at the right line.
    # TODO: record line number in
     "#{extname}"
  end

    defp editor()  , do: System.get_env("EDITOR")

    # -----

#    defp cfg_dir_exists?(path)      , do: File.dir?(path)

#    defp cfg_dir_absent?(path)      , do: ! cfg_dir_exists?(path)

#    defp cfg_name_valid?(cfg_name)  , do: ! Regex.match?(~r/[^0-9A-Za-z-_]/, cfg_name) # no punct or ws

#    defp cfg_name_invalid?(cfg_name), do: ! cfg_name_valid?(cfg_name)

#    defp cfg_exists?(cfg_name)      , do: File.exists?(cfg_file(cfg_name))

#    defp cfg_missing?(cfg_name)     , do: ! cfg_exists?(cfg_name)

    defp connected_using_tmux?()    , do: System.get_env("TMUX") != nil

    defp connected_without_tmux?()  , do: ! connected_using_tmux?

    defp has_editor?()              , do: editor() != nil

    defp missing_editor?()          , do: ! has_editor?()

end

