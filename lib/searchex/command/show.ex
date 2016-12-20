defmodule Searchex.Command.Show do

  @moduledoc false

  use Shake.Module
  alias Shake.Frame

  @doc "Module API"
  def exec(cfg_snip, tgt_id) do
    call(%Frame{cfg_snip: cfg_snip, tgt_id: String.to_integer(tgt_id)}, [])
  end

  step Searchex.Command.Results
  step :tgt_doc

  def tgt_doc(%Frame{tgt_id: tgt_id} = frame, _opts) do
    doc = Enum.at frame.results.docs, tgt_id
    %Frame{frame | tgt_doc: doc}
  end
end
