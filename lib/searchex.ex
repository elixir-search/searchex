defmodule Searchex do

  @moduledoc "App settings and metadata"

  @doc "Return the application version"
  def version do
    {:ok, "#{Util.Ext.App.version}"}
  end

  @core_base_path "~/.searchex"
  @test_base_path "test/data"

  # TODO: read the base path from ~/.searchexrc (TOML format)
  
  @doc """
  Return the base path

  In production, the base path is typically `~/.searchex`.

  The directory structure organization:

      base_dir > repo_dirs > collection_files

  There is one base dir.  Underneath the base dir there are multiple
  repo dirs.  In each repo dir, there can be multiple collection files.
  Collection files are yaml files with collection configuration parameters.
  """
  def base_dir do
    case Mix.env do
      :test -> expand(@test_base_path)
      :eval -> expand("test/data")
      _     -> expand(@core_base_path)
    end
  end

  # -----

  defp expand(path) do
    Path.expand(path)
  end
end
