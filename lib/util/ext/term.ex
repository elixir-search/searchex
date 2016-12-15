defmodule Util.Ext.Term do

  @moduledoc false

  def digest(term, length \\ 5) do
    string = :erlang.term_to_binary(term)
    :crypto.hash(:sha, string)
    |> Base.encode16
    |> String.slice(0,length)
    |> String.downcase
  end

  def to_atom(ele) when is_pid(ele)   , do: ele
  def to_atom(ele) when is_binary(ele), do: String.to_atom(ele)
  def to_atom(ele) when is_atom(ele)  , do: ele

  def join_atoms(list) do
    list
    |> Enum.map(&to_string/1)
    |> Enum.join
    |> to_atom
  end
end
