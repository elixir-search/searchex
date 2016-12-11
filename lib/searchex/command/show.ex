defmodule Searchex.Command.Show do
  @moduledoc false
#  use ExMakeOld

  # error checks:
  # - valid cfg_name
  # - existing cfg_name
  # - valid cfg
  def exec(idnum) do
    results = elem(Searchex.Command.Search.Cache.read_results,0)
    docs    = results.docs
    doc     = Enum.at(docs, String.to_integer(idnum))
    body = doc.body
    X.DIO.puts body
  end

  def handle_chain(_cfg_name) do
    "TBD"
  end
end
