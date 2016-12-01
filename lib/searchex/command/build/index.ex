defmodule Searchex.Command.Build.Index do

  @moduledoc false

  def read_or_generate(catalog) do
    start_supervisor(:index)
    if Searchex.Command.Build.Index.Cache.stale?(catalog) do
      DIO.puts "INDEX IS STALE"
      catalog |> create_from_catalog
      Searchex.Command.Build.Index.Cache.write_index(catalog)
    else
      DIO.puts "READ EXISTING INDEX"
      Searchex.Command.Build.Index.Cache.read_index(catalog)
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
      Searchex.Keyword.Server.add_keyword_position(word, docid, position)
    end
  end

  defp start_supervisor(_) do
    case Searchex.Keyword.Supervisor.start_link do
      {:ok, pid}      -> pid
      {:error, _elem} ->
        start_supervisor(:restart)
    end
    :ok
  end
end
