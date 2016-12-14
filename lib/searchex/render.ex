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
  def edit(cfg_name) do
    case Searchex.Config.edit(cfg_name) do
      {:error, msg     } -> {:error, msg}
      {:ok   , cfg_name} -> Util.EditorLaunch.launch_using_tmux(cfg_file(cfg_name))
    end
  end

  @doc """
  Invoke `Searchex.Command.search`, then render the results as a table.
  """
  def query(cfg_name, query) do
    Searchex.Command.Query.exec(cfg_name, query)
    |> Searchex.Render.Results.to_table
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
  Invoke `Searchex.Command.results`, then render the results to a table.
  """
  def results(_cfg_name) do
#    case Searchex.Command.results do
#      {:error , msg   } -> {:error, msg}
#      {:ok    , data  } -> Searchex.Render.Results.to_table(data)
#    end
    {:error, "RESULTS: UNDER CONSTRUCTION"}
  end

  @doc """
  Open the doc in an editor.
  """
  def edit(_cfg_name, _docid) do
#    case Searchex.Command.results do
#      {:error , msg   } -> {:error, msg}
#      {:ok    , data  } -> Searchex.Render.Results.to_table(data)
#    end
    {:error, "EDIT: UNDER CONSTRUCTION"}
  end



end
