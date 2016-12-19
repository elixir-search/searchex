defmodule Searchex.Config.Helpers do

  @moduledoc false

  def cfg_file(cfg_name) do
    item = match(cfg_name)
    Searchex.base_dir <> "/" <> item <> ".yml"
  end

#  def name_from_path(path) do
#    full_path = Path.expand(path)
#    _cfg_name  = String.split(full_path, "/") |> List.last
#  end

  def cache_file(cfg_name) do
    active_dirs.data <> "/" <> cfg_name <> ".dets"
  end

  # -----

  defp match(cfg_name) do
    cfg_name
    |> filter
    |> List.first
  end

  defp filter(cfg_name) do
    Searchex.Config.Ls.full
    |> Enum.filter(fn(path) -> Regex.match?(~r/#{cfg_name}/, path) end)
  end

#  defp cfg_path(_cfg_name) do
#    active_dirs.cfgs <> "/"
#  end

  defp active_dirs do
    SearchexOld.settings
  end

#  defp make_active_dirs() do
#    System.cmd("mkdir", ["-p"] ++ Map.values(active_dirs))
#  end

  # -----

  # TODO: add a test to check validity of config file...
#  def cfg_name_format(frame, _opts) do
#    frame
#  end

#  def cfg_invalid?(_cfg_name), do: {:ok}

  def cfg_nomatch?(cfg_name) do
    err = {:error, "No config file match (#{cfg_name})"}
    if match(cfg_name), do: {:ok}, else: err
  end

  def cfg_ambiguous?(cfg_name) do
    list = filter(cfg_name)
    msg  = "Multi-match (#{cfg_name} => [#{Enum.join(list, ", ")}])"
    err  = {:error, msg}
    if length(list) > 1, do: err, else: {:ok}
  end

#  def cfg_dir_absent?(path \\ SearchexOld.settings.cfgs) do
#    err = {:error, "Config dir does not exist (#{path})"}
#    if File.dir?(path), do: {:ok}, else: err
#  end

  def cfg_name_invalid?(cfg_name) do
    test = Regex.match?(~r/[^0-9A-Za-z-_\/]/, cfg_name)
    err  = {:error, "Invalid config name (#{cfg_name})"}
    if test, do: err, else: {:ok}
  end

#  def cfg_exists?(cfg_name) do
#    err = {:error, "Config already exists (#{cfg_name})"}
#    if File.exists?(cfg_file(cfg_name)), do: err, else: {:ok}
#  end

#  def cfg_missing?(cfg_name) do
#    err = {:error, "Config does not exist (#{cfg_name})"}
#    if File.exists?(cfg_file(cfg_name)), do: {:ok}, else: err
#  end

  def cfg_present?(cfg_name) do
    err = {:error, "Config already exists (#{cfg_name})"}
    if File.exists?(cfg_file(cfg_name)), do: err, else: {:ok}
  end
end
