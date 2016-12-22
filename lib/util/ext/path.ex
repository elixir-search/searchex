defmodule Util.Ext.Path do

  @moduledoc false

  def shrink_paths(paths) do
    paths
    |> Enum.map(fn(path) -> shrink_path(path) end)
    |> Enum.join(", ")
  end

  def shrink_path(path) do
    path
    |> String.split("/")
    |> Enum.split(-2)
    |> elem(1)
    |> Enum.join("/")
  end
end