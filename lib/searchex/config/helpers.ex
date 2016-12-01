defmodule Searchex.Config.Helpers do

  @moduledoc false

  def name_from_path(path) do
    full_path = Path.expand(path)
    _cfg_name  = String.split(full_path, "/") |> List.last
  end

  def active_dirs do
    Searchex.settings
  end

  def cfg_file(cfg_name) do
    active_dirs.cfgs <> "/" <> cfg_name <> ".yml"
  end

  def cat_file(cfg_name) do
    active_dirs.data <> "/" <> cfg_name <> "_cat.yml"
  end

  def idx_file(cfg_name) do
    active_dirs.data <> "/" <> cfg_name <> "_idx.yml"
  end

  def make_active_dirs() do
    System.cmd("mkdir", ["-p"] ++ Map.values(active_dirs))
  end

  def clean do
    Enum.each [active_dirs.data, active_dirs.temp], fn(x) ->
      {:ok, files} = File.ls(x)
      Enum.each files, fn(y) -> IO.puts "#{x}/#{y}" ; File.rm("#{x}/#{y}") end
    end
  end

  # -----

  def cfg_invalid?(_cfg_name), do: {:ok}

  def cfg_dir_absent?(path) do
    err = {:error, "Dir does not exist (#{path})"}
    unless File.dir?(path), do: err, else: {:ok}
  end

  def cfg_name_invalid?(cfg_name) do
    test = Regex.match?(~r/[^0-9A-Za-z-_]/, cfg_name)
    err  = {:error, "Invalid config name (#{cfg_name})"}
    unless test, do: {:ok}, else: err
  end

  def cfg_exists?(cfg_name) do
    err = {:error, "Config already exists (#{cfg_name})"}
    if File.exists?(cfg_file(cfg_name)), do: err, else: {:ok}
  end

  def cfg_missing?(cfg_name) do
    err = {:error, "Config does not exist (#{cfg_name})"}
    unless File.exists?(cfg_file(cfg_name)), do: err, else: {:ok}
  end
end
