defmodule Searchex.Command.Build.Index do

  @moduledoc false

  def map_to_otp(map, pt_name) do
    start_supervisor(pt_name)
    add_supervisor_to_lru_registry(pt_name)
    Searchex.Keyword.Supervisor.map_to_otp(map, pt_name)
    :ok
  end

  def otp_to_map(pt_name) do
    Searchex.Keyword.Supervisor.otp_to_map(pt_name)
  end

  def create_from_frame(frame, pt_name) do
    start_supervisor(pt_name)
    add_supervisor_to_lru_registry(pt_name)
    frame.catalog.docs
    |> Enum.map(fn(doc) -> process_doc(doc, pt_name) end)
    |> Enum.map(fn(x)   -> Task.await(x, 5_000) end)
    :ok
  end

  defp process_doc(doc, pt_name) do
    Task.async fn() ->
      docid = doc.docid
      doc.wordstems
      |> Enum.with_index(1)
      |> Enum.map(fn({word, pos}) -> {pt_name, docid, word, pos} end)
      |> Enum.map(&process_word/1)
    end
  end

  defp process_word({pt_name, docid, word, position}) do
    Searchex.Keyword.Server.add_keyword_position(pt_name, word, docid, position)
  end

  defp start_supervisor(pt_name) do
    catom = Util.Ext.Term.to_atom(pt_name)
    case Searchex.Keyword.Supervisor.start_link(catom) do
      {:ok,     pid} -> pid
      {:error, _val} -> catom
    end
    Searchex.Keyword.Supervisor.remove_all_otp_children(catom)
    :ok
  end

  # TODO: add the supervisor to the LRU Registry
  defp add_supervisor_to_lru_registry(_pt_name) do
    "TBD"
  end
end
