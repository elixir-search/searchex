defmodule Searchex.Command.Build.Index do

  @moduledoc false

  def create_from_catalog(catalog) do
    TIO.inspect catalog, color: "BLUE"
    catalog.docs
    |> Enum.map(&process_doc/1)
    |> Enum.map(fn(x) -> Task.await(x, 1_000) end)
    :ok
  end

  def process_doc(doc) do
#    TIO.inspect "PD"  , color: "BLUE"
    col = X.Term.to_atom(doc.params.collection)
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
