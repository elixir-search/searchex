defmodule Searchex.Command.CatalogTest do
  use ExUnit.Case

  import Searchex.Command.Catalog

  setup :cleanup

  describe "min collection" do
    test "doc count" do
      {:ok, _timestamp, scan} = exec("min")
      assert scan.numdocs == 0
    end
  end

  describe "multi collection" do
    test "doc count" do
      {:ok, _timestamp, scan} = exec("multi")
      assert scan.numdocs == 7
    end
  end

  describe "tweets collection" do
    test "doc count" do
      {:ok, _timestamp, scan} = exec("tweets")
      assert scan.numdocs == 0
    end
  end

  describe "worklog collection" do
    test "doc count" do
      {:ok, _timestamp, scan} = exec("worklog")
      assert scan.numdocs == 7
    end
  end

  # remove all cached products...
  defp cleanup(_) do
    Searchex.Command.clean
    :ok
  end
end
