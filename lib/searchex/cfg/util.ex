defmodule Searchex.Cfg.Util do

  @moduledoc false

    @doc "Return the CFG data as a Map"
    def to_map({:ok, yaml_data}) do
      YamlElixir.read_from_string yaml_data, atoms: true
    end

    def to_map(cfg_name) do
      {:ok, yaml_data} = Searchex.Cfg.cfg_cat(cfg_name)
      YamlElixir.read_from_string yaml_data
    end
end