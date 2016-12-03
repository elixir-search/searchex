defmodule Searchex.Command.CatalogTest do
  use ExUnit.Case, async: true

  import Searchex.Command.Catalog

  setup :cleanup

  describe "tweets collection" do
    test "basic operation" do
      result = exec("worklog")
      TIO.inspect result, label: "BANG", color: "blue"
      assert 1 == 1
    end
  end

  # remove all cached products...
  defp cleanup(_) do
    Searchex.Command.clean
    :ok
  end
end
