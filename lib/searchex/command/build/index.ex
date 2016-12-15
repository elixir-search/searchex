defmodule Searchex.Command.Build.Index do

  @moduledoc false

  # TODO: use the catalog digest as the supervisor process name
  def create_from_frame(frame) do
    cfg_name = frame.cfg_name
    start_supervisor(cfg_name)
    add_supervisor_to_lru_registry(cfg_name)
    frame.catalog.docs
    |> Enum.map(fn(doc) -> process_doc(doc, cfg_name) end)
    |> Enum.map(fn(x)   -> Task.await(x, 5_000) end)
    :ok
  end

  def process_doc(doc, cfg_name) do
    Task.async fn() ->
      docid = doc.docid
      doc.wordstems
      |> Enum.with_index(1)
      |> Enum.map(fn({word, pos}) -> {cfg_name, docid, word, pos} end)
      |> Enum.map(&process_word/1)
    end
  end

  def process_word({cfg_name, docid, word, position}) do
    Searchex.Keyword.Server.add_keyword_position(cfg_name, word, docid, position)
  end

  def start_supervisor(cfg_name) do
    catom = Util.Ext.Term.to_atom(cfg_name)
    case Searchex.Keyword.Supervisor.start_link(catom) do
      {:ok,     pid} -> pid
      {:error, _val} -> catom
    end
    Searchex.Keyword.Supervisor.remove_all_otp_children(catom)
    :ok
  end

  # TODO: add the supervisor to the LRU Registry
  def add_supervisor_to_lru_registry(_cfg_name) do
    "TBD"
  end
end
