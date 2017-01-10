defmodule Searchex.Render do
  @moduledoc """
  CLI rendering : tables and editor launch

  - editor launching
  - rendering search-results as a table
  - mini-views for build steps
  """

  import Searchex.Config.CfgHelpers

  @doc """
  Invoke `Searchex.Command.modify`, then launch an editor to open the config file.

  NOTE: you must define environment variables `EDITOR`.  This will only work with TMUX

  Editor-launch in Elixir needs fixing.  See this thread for more info:
  https://elixirforum.com/t/how-to-launch-an-editor-from-escript/2094/1
  """
  def modify(cfg_snip) do
    case Searchex.Config.edit(cfg_snip) do
      {:errMor, msg     } -> {:error, msg}
      {:ok   , cfg_snip} -> Util.EditorLaunch.launch_using_tmux(cfg_file(cfg_snip))
    end
  end

  @doc """
  Return processing summary for catalog generation.
  """
  def catalog(cfg_snip) do
    frame = Searchex.Command.catalog(cfg_snip)
    if frame.halted do
      {:error, frame.halt_msg}
    else
      [cmd: "catalog", cfg_name: frame.cfg_name, numdocs: frame.catalog.numdocs, file_roots: frame.params.file_roots]
    end
  end

  @doc """
  Return processing summary for index generation.
  """
  def index(cfg_snip) do
    frame = Searchex.Command.index(cfg_snip)
    if frame.halted do
      {:error, frame.halt_msg}
    else
      [cmd: "index", cfg_name: frame.cfg_name]
    end
  end

  @doc """
  Return processing summary for build.  (Catalog + Index)
  """
  def build(cfg_snip) do
    frame = Searchex.Command.build(cfg_snip)
    if frame.halted do
      {:error, frame.halt_msg}
    else
      [cmd: "build", cfg_name: frame.cfg_name, numdocs: frame.catalog.numdocs, file_roots: frame.params.file_roots]
    end
  end

  @doc """
  Invoke `Searchex.Command.query`, then render the results as a table.
  """
  def query(cfg_snip, query) do
    frame = Searchex.Command.query(cfg_snip, query)
    if frame.halted do
      {:error, frame.halt_msg}
    else
      Searchex.Render.Results.to_table(frame)
    end
  end

  @doc """
  Invoke `Searchex.Command.results`, then render the results to a table.
  """
  def results(cfg_snip) do
    frame = Searchex.Command.results(cfg_snip)
    if frame.halted do
      {:error, frame.halt_msg}
    else
      Searchex.Render.Results.to_table(frame)
    end
  end

  @doc """
  Show document text.
  """
  def show(cfg_snip, tgt_id) do
    frame = Searchex.Command.show(cfg_snip, tgt_id)
    if frame.halted do
      {:error, frame.halt_msg}
    else
      frame.tgt_doc.body
    end
  end

  @doc """
  Edit a document.  

  For files which contain multiple documents, this command attempts to launch
  the editor on the right line.

  NOTE: you must define environment variables `EDITOR`.  This will only work with TMUX

  Editor-launch in Elixir needs fixing.  See this thread for more info:
  https://elixirforum.com/t/how-to-launch-an-editor-from-escript/2094/1
  """
  def edit(cfg_snip, tgt_id) do
    frame = Searchex.Command.show(cfg_snip, tgt_id)
    if frame.halted do
      {:error, frame.halt_msg}
    else
      Util.EditorLaunch.launch_using_tmux(frame.tgt_doc.filename, frame.tgt_doc.startline)
    end
  end

  @doc """
  Show statistics for all collections.
  """
  def info do
    Searchex.Config.Ls.exec
    |> elem(1)
    |> Enum.map(fn(cfg) -> info(cfg) end)
    |> Searchex.Render.Info.to_table
  end

  defp info(cfg_snip) do
    alias Searchex.Command.CmdHelpers
    frame = Searchex.Command.info(cfg_snip)
    if frame.halted do
      {:error, frame.halt_msg}
    else
      doc_size   = CmdHelpers.doc_size(frame)
      cache_size = CmdHelpers.cache_size(frame)
      [
        cmd:        "info"                                     ,
        cfg_name:   frame.cfg_name                             ,
        numdocs:    frame.catalog.numdocs                      ,
        doc_size:   Util.Ext.Integer.format(doc_size  )        ,
        cache_size: Util.Ext.Integer.format(cache_size)      #  ,
#        file_roots: Util.Ext.Path.shrink_paths(frame.params.file_roots)
      ]
    end
  end

  @doc """
  Remove the cache file for a collection.
  """
  def clean(cfg_snip) do
    frame = Searchex.Command.Params.exec(cfg_snip)
    if frame.halted do
      {:error, frame.halt_msg}
    else
      file  = Searchex.Command.CmdHelpers.cache_file(frame)
      if File.exists?(file), do: File.rm!(file)
      {:ok}
    end
  end
end
