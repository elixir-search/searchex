defmodule Searchex.Config do

  @moduledoc """
  Manage Searchex configuration files
  
  Configurations are stored in yaml files under `~/.Searchex.Configs`.
  """

  @doc "Create a new config"
  def new(path) do
    Searchex.Config.New.exec(path)
  end

  @doc "Fetch a config from elixir-search/sample_docs"
  def sample_fetch(_tmp) do
    Util.Ext.IO.puts "FETCH : UNDER CONSTRUCTION"
    {:error, "Pending Implementation"}
  end

  @doc "Fetch a config from elixir-search/sample_docs"
  def sample_ls do
    Util.Ext.IO.puts "LS : UNDER CONSTRUCTION"
    {:error, "Pending Implementation"}
  end

  @doc "Return the contents of a config"
  def cat_old(cfg_snip) do
    Searchex.Config.CatOld.exec(cfg_snip)
  end

  @doc "Return the contents of a config"
  def cat(cfg_snip) do
    Searchex.Config.Cat.exec(cfg_snip)
  end

  @doc "Return information needed to edit the config"
  def edit(cfg_snip) do
    Searchex.Config.Edit.exec(cfg_snip)
  end

  @doc "Remove a config"
  def rm(cfg_snip) do
    Searchex.Config.Rm.exec(cfg_snip)
  end

  @doc "List the configs"
  def ls_old do
    Searchex.Config.LsOld.exec
  end

  @doc "List the configs"
  def ls do
    Searchex.Config.Ls.exec
  end
end
