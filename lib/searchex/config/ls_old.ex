defmodule Searchex.Config.Ls do
  @moduledoc false

  import Searchex.Config.Helpers

  def exec do
    make_active_dirs()
    {files, _code} = System.cmd("ls", [], cd: active_dirs.cfgs)
    output = files
             |> String.split("\n")
             |> Enum.filter(&(Regex.match?(~r/yml$/, &1)))
             |> Enum.map(&(String.replace(&1, ~r/.yml$/, "")))
    {:ok, output}
  end
end