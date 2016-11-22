defmodule Searchex.Build.Index do

  @moduledoc false

  alias Searchex.Util.IO, as: DO

  def read_or_generate(catalog) do
    start_supervisor(:index)
    if Searchex.Build.Index.Cache.stale?(catalog) do
      DO.puts "INDEX IS STALE"
      catalog |> create_from_catalog
      Searchex.Build.Index.Cache.write_index(catalog)
    else
      DO.puts "READ EXISTING INDEX"
      Searchex.Build.Index.Cache.read_index(catalog)
    end
  end

  def create_from_catalog(catalog) do
    catalog.docs
    |> Enum.map(&process_doc/1)
    |> Enum.map(fn(x) -> Task.await(x, 60000) end)
    :ok
  end

  def process_doc(doc) do
    Task.async fn() ->
      docid = doc.docid
      doc.wordstems
      |> Enum.with_index(1)
      |> Enum.map(fn({word, pos}) -> {docid, word, pos} end)
      |> Enum.map(&process_word/1)
      |> Enum.map(fn(x) -> Task.await(x, 60000) end)
    end
  end

  def process_word({docid, word, position}) do
    Task.async fn() ->
      Searchex.KeywordSer.add_keyword_position(word, docid, position)
    end
  end

  defp start_supervisor(_) do
    case Searchex.KeywordSup.start_link do
      {:ok, pid}      -> pid
      {:error, _elem} ->
        #IO.inspect(START_SUP_ERROR_C: elem)
        start_supervisor(:restart)
    end
    :ok
  end
end
