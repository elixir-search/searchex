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

  def expanded_file_paths(frame) do
    Enum.map frame.file_paths, fn(path) -> Path.expand(path, repo_dir(frame)) end
  end

  def file_list(frame) do
    frame.params.file_paths
    |> Enum.map(fn(path) -> Path.expand(path, repo_dir(frame)) end)
    |> Enum.map(fn(path) -> expand(path) end)
    |> List.flatten
  end

  # -----

  defp expand(path) do
    if File.dir?(path) do
      Util.Ext.File.ls_r(path)
    else
      path
    end
  end
end
