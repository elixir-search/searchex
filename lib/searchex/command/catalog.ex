defmodule Searchex.Command.Catalog do
  use ExMake

  # error checks:
  # - valid cfg_name
  # - existing cfg_name
  # - valid cfg
  def exec(cfg_name) do
      Searchex.Command.Build.Catalog.assemble(cfg_name)
  end

  def handle_chain(_cfg_name) do
    "TBD"
  end
end