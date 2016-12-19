defmodule Searchex.Config.Helpers do

  @moduledoc false

  def cfg_file(cfg_snip) do
    Searchex.base_dir <> "/" <> cfg_name(cfg_snip) <> ".yml"
  end

  def cache_file(cfg_snip) do
    Searchex.base_dir <> "/_" <> cfg_name(cfg_snip) <> ".dets"
  end

  def cfg_name(cfg_snip) do
    cfg_snip
    |> filter
    |> List.first
  end

  # -----

  defp filter(cfg_snip) do
    pattern = String.split(cfg_snip, "") |> Enum.split(-1) |> elem(0) |> Enum.join(".*")
    Searchex.Config.Ls.full
    |> Enum.filter(fn(path) -> Regex.match?(~r/#{pattern}/, path) end)
  end

#  defp active_dirs do
#    SearchexOld.settings
#  end

  # -----

  # TODO: add a test to check validity of config file...

  def cfg_nomatch?(cfg_snip) do
    err = {:error, "No config file match (#{cfg_snip})"}
    if cfg_name(cfg_snip), do: {:ok}, else: err
  end

  def cfg_ambiguous?(cfg_snip) do
    list = filter(cfg_snip)
    msg  = "Multi-match (#{cfg_snip} => [#{Enum.join(list, ", ")}])"
    err  = {:error, msg}
    if length(list) > 1, do: err, else: {:ok}
  end

  def cfg_snip_invalid?(cfg_snip) do
    test = Regex.match?(~r/[^0-9A-Za-z-_\/]/, cfg_snip)
    err  = {:error, "Invalid config name (#{cfg_snip})"}
    if test, do: err, else: {:ok}
  end

  def cfg_present?(cfg_snip) do
    err = {:error, "Config already exists (#{cfg_snip})"}
    if File.exists?(cfg_file(cfg_snip)), do: err, else: {:ok}
  end
end
