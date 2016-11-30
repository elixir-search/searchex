defmodule Searchex.Render do
  @moduledoc """
  CLI rendering for anything more sophisticated than `IO.puts`

  - editor launching
  - rendering search-results as a table
  """

  import Searchex.Config.Helpers

  @doc """
  Invoke `Searchex.Command.cfg_edit`, then launch an editor to open the config file.

  NOTE: you must define environment variables `EDITOR`.  This will only work with TMUX.

  This needs fixing.  See this thread for more info:
  https://elixirforum.com/t/how-to-launch-an-editor-from-escript/2094/1
  """
  def cfg_edit(cfg_name) do
    case Searchex.Config.cfg_edit(cfg_name) do
      {:error, msg     } -> {:error, msg}
      {:ok   , cfg_name} -> EditorLaunch.using_tmux(cfg_file(cfg_name))
    end
  end

  @doc """
  Invoke `Searchex.Command.search`, then render the results as a table.
  """
  def search(cfg_name, query) do
    case Searchex.Command.search(cfg_name, query) do
      {:error, msg      } -> {:error, msg}
      {:ok   , params   } -> Searchex.Render.Results.to_table(params)
    end
  end

  @doc """
  Alias for `search`...
  """
  def query(cfg_name, query) do
    search(cfg_name, query)
  end

  @doc """
  Invoke `Searchex.Command.results`, then render the results to a table.
  """
  def results do
    case Searchex.Command.results do
      {:error , msg     } -> {:error, msg}
      {:ok    , params  } -> Searchex.Render.Results.to_table(params)
    end
  end
end