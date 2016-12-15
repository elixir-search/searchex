defmodule Searchex do

  @moduledoc "App settings and metadata"

  @doc "Return the application version"
  def version do
    {:ok, "#{Util.Ext.App.version}"}
  end

  @doc "Return the applications settings"
  def settings do

    # TODO: read these from ~/.searchexrc (TOML format) and store in a GenServer...

    case Mix.env do
      :test -> expand(test_settings)
      :eval -> expand(test_settings)
      _     -> expand(base_settings)
    end
  end

  # -----------------------------------------------------------------------------------

  defp test_settings do
    %{
      cfgs: "test/data/configs"          ,
      docs: "/tmp/searchex_test/docs"    ,
      data: "/tmp/searchex_test/data"    ,
    }
  end

  defp base_settings do
    %{
      cfgs: "~/.searchex/cfgs"   ,
      docs: "~/.searchex/docs"   ,
      data: "~/.searchex/data"   ,
    }
  end

  defp expand(map) do
    Enum.reduce(map, %{}, fn({k,v}, acc) -> Map.merge(acc, %{k => Path.expand(v)}) end)
  end
end
