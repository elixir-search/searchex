defmodule Searchex.Render do
  @moduledoc """
  CLI rendering : tables and editor launch

  - editor launching
  - rendering search-results as a table
  """

  import Searchex.Config.Helpers

  @doc """
  Invoke `Searchex.Command.edit`, then launch an editor to open the config file.

  NOTE: you must define environment variables `EDITOR`.  This will only work with TMUUtil.

  This needs fixing.  See this thread for more info:
  https://elixirforum.com/t/how-to-launch-an-editor-from-escript/2094/1
  """
  def modify(cfg_name) do
    case Searchex.Config.edit(cfg_name) do
      {:error, msg     } -> {:error, msg}
      {:ok   , cfg_name} -> Util.EditorLaunch.launch_using_tmux(cfg_file(cfg_name))
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
      [cmd: "catalog", cfg_name: cfg_name, numdocs: frame.catalog.numdocs, doc_dirs: frame.params.doc_dirs]
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
      [cmd: "build", cfg_name: cfg_name, numdocs: frame.catalog.numdocs, doc_dirs: frame.params.doc_dirs]
    end
  end

  @doc """
  Invoke `Searchex.Command.search`, then render the results as a table.
  """
  def query(cfg_name, query) do
    frame = Searchex.Command.query(cfg_name, query)
    if frame.halted do
      {:error, frame.halt_msg}
    else
      Searchex.Render.Results.to_table(frame)
    end
  end

  @doc """
  Invoke `Searchex.Command.results`, then render the results to a table.
  """
  def results(cfg_name) do
    frame = Searchex.Command.results(cfg_name)
    if frame.halted do
      {:error, frame.halt_msg}
    else
      Searchex.Render.Results.to_table(frame)
    end
  end

  @doc """
  Show
  """
  def show(cfg_name, tgt_id) do
    frame = Searchex.Command.show(cfg_name, tgt_id)
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
  def info(cfg_name) do
    frame = Searchex.Command.info(cfg_name)
    if frame.halted do
      {:error, frame.halt_msg}
    else
      doc_size   = Util.Ext.File.du_s(frame.params.doc_dirs)
      cache_size = Util.Ext.File.du_s(Searchex.settings[:data] <> "/#{cfg_name}.dets")
      [
        cmd:        "info"                              ,
        cfg_name:   cfg_name                            ,
        numdocs:    frame.catalog.numdocs               ,
        doc_size:   Util.Ext.Integer.format(doc_size  ) ,
        cache_size: Util.Ext.Integer.format(cache_size)
      ]
    end
  end
end
