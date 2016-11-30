defmodule Searchex.Command.Index do
  use ExMake

  # error checks:
  # - valid cfg_name
  # - existing cfg_name
  # - valid cfg
  def exec(cfg_name) do
    catalog = Searchex.Command.Catalog.exec(cfg_name)
    Searchex.Command.Build.Index.read_or_generate(catalog)
    catalog
  end

  def handle_chain(_cfg_name) do
    "TBD"
  end
end