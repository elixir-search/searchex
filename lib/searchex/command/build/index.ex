defmodule Searchex.Command.Build.Index do

  @moduledoc false

  def create_from_catalog(catalog) do
    catalog.docs
    |> Enum.map(&process_doc/1)
    |> Enum.map(fn(x) -> Task.await(x, 1_000) end)
    :ok
  end

  def process_doc(doc) do
    TIO.inspect "PD"  , color: "BLUE"
    Task.async fn() ->
      docid = doc.docid
      doc.wordstems
      |> Enum.with_index(1)
      |> Enum.map(fn({word, pos}) -> {docid, word, pos} end)
      |> Enum.map(&process_word/1)
      |> Enum.map(fn(x) -> Task.await(x, 1_000) end)
    end
  end

  def process_word({docid, word, position}) do
    Task.async fn() ->
      Searchex.Keyword.Server.add_keyword_position(word, docid, position)
    end
  end

  def start_supervisor(name) do
    case Searchex.Keyword.Supervisor.start_link do
      {:ok, pid}      -> pid
      {:error, _elem} -> start_supervisor(name)
    end
    :ok
  end
end
