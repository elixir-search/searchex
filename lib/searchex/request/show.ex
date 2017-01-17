defmodule Searchex.Request.Show do

  @moduledoc false

  use Shreq.Module
  alias Shreq.Frame

  @doc "Module API"
  def exec(cfg_snip, tgt_id) do
    call(%Frame{cfg_snip: cfg_snip, tgt_id: tgt_id}, [])
  end

  step Searchex.Request.Results
  step :integer_tgt_id?
  step :valid_tgt_id?
  step :tgt_doc

  def integer_tgt_id?(%Frame{tgt_id: tgt_id} = frame, _opts) do
    case Integer.parse(tgt_id) do
      :error          -> halt(frame, "Not a number (#{tgt_id})")
      {new_tgt_id, _} -> %Frame{frame | tgt_id: new_tgt_id}
    end
  end

  def valid_tgt_id?(%Frame{tgt_id: tgt_id} = frame, _opts) do
    maxid = length(frame.results.docs)
    cond do
      tgt_id < 1     -> halt(frame, "Invalid ID (#{tgt_id})")
      tgt_id > maxid -> halt(frame, "Invalid ID (#{tgt_id})")
      true           -> frame
    end
  end

  def tgt_doc(%Frame{tgt_id: tgt_id} = frame, _opts) do
    doc = Enum.at frame.results.docs, (tgt_id - 1)
    %Frame{frame | tgt_doc: doc}
  end
end
