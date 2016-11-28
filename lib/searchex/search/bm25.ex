defmodule Searchex.Search.Bm25 do

  @moduledoc false

  @num_docs          100
  @avg_doc_token_len 100
  @doc_token_len     100

  @term_freq_tuning_factor 1.2
  @doc_len_tuning_param    0.72

  @doc """
  Document Scores

  terms looks like:
  ["term1", "term2"]

  doc_matches looks like:
  [{"term1", %{"docid1" => [pos1, pos2], "docid2" => [pos3, pos4]}}, {"term2", %{}}]

  matches_per_term_and_doc looks like:
  %{{"term1", "docid1"} => 23, {"term1", "docid2"} => 4, ...}
  """
  def doc_scores(terms, doc_matches, matches_per_term_and_doc) do
    docids = uniq_doc_ids(doc_matches)
    tuples = Enum.map docids, fn(docid) ->
      {docid, doc_score(terms, docid, doc_matches, matches_per_term_and_doc, @avg_doc_token_len, @num_docs)}
    end
    Enum.sort tuples, &(elem(&1,1) > elem(&2,1))
  end

  @doc """
  Document Score
  """
  def doc_score(terms, docid, doc_matches, matches_per_term_and_doc, avg_doc_len, num_docs) do
    term_scores = Enum.map terms, fn(term) ->
      match_count = case matches_per_term_and_doc[{term, docid}] do
        nil   -> 0
        count -> count
      end
      term_doc_score(match_count,
                     num_docs_with_term(term, doc_matches),
                     avg_doc_len(@doc_token_len, avg_doc_len),
                     num_docs)
    end
    Enum.sum(term_scores)
  end

  @doc """
  Term Document Score

  ## Examples

      iex> Searchex.Search.Bm25.term_doc_score(2, 10, 150, 200)
      0.07362

  """
  def term_doc_score(num_times_in_doc, num_docs_with_term, avg_doc_len, num_docs) do
    term_freq = term_freq(num_times_in_doc, avg_doc_len)
    idf       = inverse_doc_freq_of_term(num_docs, num_docs_with_term)
    result      = (term_freq * idf)
    if is_float(result), do: Float.round(result, 5), else: result
  end


  @doc """
  Unique Document IDs

  ## Example

      iex> Searchex.Search.Bm25.uniq_doc_ids([{"term1", %{"docid1" => [2,4,5]}}])
      ["docid1"]

  """
  def uniq_doc_ids(doc_matches) do
    doc_matches
    |> Enum.reduce([], fn({_term, map}, acc) -> acc ++ Map.keys(map) end)
    |> List.flatten
    |> Enum.uniq
  end

  @doc """
  Number of Documents with Term

  ## Examples

      iex> Searchex.Search.Bm25.inverse_doc_freq_of_term(:NA, 0)
      0

  """
  def num_docs_with_term(term, doc_matches) do
    case :lists.keyfind(term, 1, doc_matches) do
      {_, doc_ids} -> Enum.count(doc_ids)
      false        -> 0
    end
  end

  @doc """
  Inverse Document Frequency of Term

  ## Examples

      iex> Searchex.Search.Bm25.inverse_doc_freq_of_term(:NA, 0)
      0

      iex> Searchex.Search.Bm25.inverse_doc_freq_of_term(100, 5)
      3.04452
  """
  def inverse_doc_freq_of_term(_, 0) do
    0
  end

  def inverse_doc_freq_of_term(total_num_docs, num_docs_with_term) do
    :math.log(1.0 + total_num_docs / num_docs_with_term)
    |> Float.round(5)
  end

  @doc """
  Term Frequency

  ## Examples

      iex> Searchex.Search.Bm25.term_freq(0, :NA)
      0

      iex> Searchex.Search.Bm25.term_freq(5, 20)
      0.37931

"""
  def term_freq(0, _AverageDocumentLength) do
    0
  end
  def term_freq(num_appearances_in_doc, avg_doc_len) do
    (num_appearances_in_doc * (@term_freq_tuning_factor + 1)) /
            (num_appearances_in_doc + avg_doc_len * @term_freq_tuning_factor)
    |> Float.round(5)
  end

  @doc """
  Average Document Length

  ## Example

      iex> Searchex.Search.Bm25.avg_doc_len(20, 40)
      0.64

  """
  def avg_doc_len(doc_len_in_words, avg_doc_len_in_words) do
    (1 - @doc_len_tuning_param) + @doc_len_tuning_param * doc_len_in_words / avg_doc_len_in_words
  end

end
