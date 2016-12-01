defmodule Searchex.Command.Build do

#  use ExMake

  # error checks:
  # - valid cfg_name
  # - existing cfg_name
  # - valid cfg
  def exec(cfg_name) do
    Searchex.Command.Index.exec(cfg_name)
  end

  def handle_chain(_cfg_name) do
    "TBD"
  end
end