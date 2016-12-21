defmodule Searchex.Config.CfgHelpers do

  @moduledoc false

  def cfg_file(cfg_snip) do
    Searchex.base_dir <> "/" <> cfg_name(cfg_snip) <> ".yml"
  end

  def cache_file(cfg_snip) do
    Searchex.base_dir <> "/_" <> cfg_name(cfg_snip) <> ".dets"
  end

  def cfg_name(cfg_snip) do
    cfg_snip
    |> cfg_filter
    |> List.first
  end

  def cfg_filter(cfg_snip) do
    pattern = String.split(cfg_snip, "") |> Enum.split(-1) |> elem(0) |> Enum.join(".*")
    Searchex.Config.Ls.full
    |> Enum.filter(fn(path) -> Regex.match?(~r/#{pattern}/, path) end)
  end
end
