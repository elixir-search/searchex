defmodule X.Term do
  def digest(term, length \\ 5) do
    string = :erlang.term_to_binary(term)
    :crypto.hash(:sha, string)
    |> Base.encode16
    |> String.slice(0,length)
    |> String.downcase
  end
end