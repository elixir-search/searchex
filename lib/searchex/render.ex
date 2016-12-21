defmodule Searchex.Render do
  @moduledoc """
  CLI rendering : tables and editor launch

  - editor launching
  - rendering search-results as a table
  """

  import Searchex.Config.CfgHelpers

  @doc """
  Invoke `Searchex.Command.edit`, then launch an editor to open the config file.

  NOTE: you must define environment variables `EDITOR`.  This will only work with TMUUtil.

  This needs fixing.  See this thread for more info:
  https://elixirforum.com/t/how-to-launch-an-editor-from-escript/2094/1
  """
  def modify(cfg_snip) do
    case Searchex.Config.edit(cfg_snip) do
      {:errMor, msg     } -> {:error, msg}
      {:ok   , cfg_snip} -> Util.EditorLaunch.launch_using_tmux(cfg_file(cfg_snip))
    end
  end

  @doc """
  Return output for catalog
  """
  def catalog(cfg_name) do
    frame = Searchex.Command.catalog(cfg_name)
    if frame.halted do
      {:error, frame.halt_msg}
    else
      [cmd: "catalog", cfg_name: cfg_name, numdocs: frame.catalog.numdocs, file_paths: frame.params.file_paths]
    end
  end

  @doc """
  Index
  """
  def index(cfg_name) do
    frame = Searchex.Command.index(cfg_name)
    if frame.halted do
      {:error, frame.halt_msg}
    else
      [cmd: "index", cfg_name: cfg_name]
    end
  end

  @doc """
  Build
  """
  def build(cfg_name) do
    frame = Searchex.Command.build(cfg_name)
    if frame.halted do
      {:error, frame.halt_msg}
    else
      [cmd: "build", cfg_name: cfg_name, numdocs: frame.catalog.numdocs, file_paths: frame.params.file_paths]
    end
  end

  @doc """
  Invoke `Searchex.Command.search`, then render the results as a table.
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
  Show
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
  Edit
  """
  def edit(cfg_name, tgt_id) do
    frame = Searchex.Command.show(cfg_name, tgt_id)
    if frame.halted do
      {:error, frame.halt_msg}
    else
      Util.EditorLaunch.launch_using_tmux(frame.tgt_doc.filename, frame.tgt_doc.startline)
    end
  end

  @doc """
  Info
  """
  def info do
#    {:ok, configs} = Searchex.Config.Ls.exec
    Searchex.Config.Ls.exec
    |> elem(1)
    |> Enum.map(fn(cfg) -> Task.async(fn -> info(cfg) end) end)
    |> Enum.map(fn(worker) -> Task.await(worker) end)
    |> Searchex.Render.Info.to_table
  end

  def info(cfg_snip) do
    alias Searchex.Command.CmdHelpers
    frame = Searchex.Command.info(cfg_snip)
    if frame.halted do
      {:error, frame.halt_msg}
    else
      doc_size   = Util.Ext.File.du_s(frame.params.file_paths)
      cache_size = Util.Ext.File.du_s(CmdHelpers.cache_file(frame))
      [
        cmd:        "info"                              ,
        cfg_name:   frame.cfg_name                      ,
        numdocs:    frame.catalog.numdocs               ,
        doc_size:   Util.Ext.Integer.format(doc_size  ) ,
        cache_size: Util.Ext.Integer.format(cache_size)
      ]
    end
  end
end
