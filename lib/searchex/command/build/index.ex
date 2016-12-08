defmodule Searchex.Command.Build.Index do

  @moduledoc false

  def create_from_catalog(catalog) do
    col = X.Term.to_atom(catalog.params.collection)
    start_supervisor(col)
    Searchex.Kw.Supervisor.remove_all_otp_children(col)
    catalog.docs
    |> Enum.map(fn(doc) -> process_doc(doc, col) end)
    |> Enum.map(fn(x)   -> Task.await(x, 1_000) end)
    :ok
  end

  def process_doc(doc, col) do
    Task.async fn() ->
      docid = doc.docid
      doc.wordstems
      |> Enum.with_index(1)
      |> Enum.map(fn({word, pos}) -> {col, docid, word, pos} end)
      |> Enum.map(&process_word/1)
      |> Enum.map(fn(x) -> Task.await(x, 1_000) end)
    end
  end

  def process_word({col, docid, word, position}) do
    Task.async fn() ->
      Searchex.Kw.Server.add_keyword_position(col, word, docid, position)
    end
  end

  def start_supervisor(col) do
    case Searchex.Kw.Supervisor.start_link(col) do
      {:ok, pid}      -> pid
      {:error, _elem} -> start_supervisor(col)
    end
    :ok
  end
end
