defmodule Searchex.Cfg do

  @moduledoc """
  Manage Searchex configuration files
  
  Configurations are stored in yaml files under `~/.searchex/cfgs`.
  """

  @doc "Create a new config"
  def cfg_new(path) do
    Searchex.Cfg.New.exec(path)
  end

  @doc "Fetch a config from elixir-search/sample_docs"
  def cfg_fetch(_tmp) do
    DIO.puts "FETCH : UNDER CONSTRUCTION"
    {:error, "Pending Implementation"}
  end

  @doc "Return the contents of a config"
  def cfg_cat(cfg_name) do
    Searchex.Cfg.Cat.exec(cfg_name)
  end

  @doc """
  Launch an editor to update a config.  NOTE: you must define environment
  variables `EDITOR`.  This will only work with TMUX.

  This needs fixing.  See this thread for more info:
  https://elixirforum.com/t/how-to-launch-an-editor-from-escript/2094/1
  """
  def cfg_edit(cfg_name) do
    Searchex.Cfg.Edit.exec(cfg_name)
  end

  @doc "Remove a config"
  def cfg_rm(cfg_name) do
    Searchex.Cfg.Rm.exec(cfg_name)
  end

  @doc "List the configs"
  def cfg_ls do
    Searchex.Cfg.Ls.exec
  end
end
