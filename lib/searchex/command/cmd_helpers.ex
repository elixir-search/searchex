defmodule Searchex.Command.CmdHelpers do

  @moduledoc false

  def repo_dir(frame) do
    repo_name = frame.cfg_name |> String.split("/") |> Enum.at(0)
    "#{Searchex.base_dir}" <> "/" <> repo_name
  end

  def cache_dir(frame) do
    Path.expand frame.params.cache_dir, repo_dir(frame)
  end

  def cache_file(frame) do
    file_base = String.replace(frame.cfg_name, "/", "_")
    cache_dir(frame) <> "/_#{file_base}.dets"
  end

  def cfg_file(frame) do
    Searchex.base_dir <> "/" <> frame.cfg_name <> ".yml"
  end

  def expanded_file_roots(frame) do
    Enum.map frame.params.adapter.file_roots, fn(path) -> Path.expand(path, repo_dir(frame)) end
  end

  def doc_size(frame) do
    roots = expanded_file_roots(frame)
    Util.Ext.File.du_s(roots, Searchex.Adapter.Type.Filesys.file_params(frame.params))
  end

  def cache_size(frame) do
    Util.Ext.File.du_s([cache_file(frame)], %{})
  end
end
