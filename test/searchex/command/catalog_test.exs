defmodule Searchex.Command.CatalogTest do
  use ExUnit.Case

  import Searchex.Command.Catalog

#  setup :cleanup

  describe "min collection" do
    test "doc count" do
      frame = exec("min")
      assert frame.catalog.numdocs == 10
    end
  end

  describe "multi collection" do
    test "doc count" do
      frame = exec("multi")
      assert frame.catalog.numdocs == 20
    end
  end

  describe "tweets collection" do
    test "doc count" do
      frame = exec("tweets")
      assert frame.catalog.numdocs == 10
    end
  end

  describe "worklog collection" do
    test "doc count" do
      frame = exec("worklog")
      assert frame.catalog.numdocs == 7
    end
  end

#  describe "multiple runs" do
#    test "doc count" do
#      frame1 = exec("min")
#      assert :ets.info(:ex_cache_ets, :size) == 1
#      {:ok, {_scan2, _digest2}} = exec("multi")
#      assert :ets.info(:ex_cache_ets, :size) == 2
#      {:ok, {_scan3, _digest3}} = exec("tweets")
#      assert :ets.info(:ex_cache_ets, :size) == 3
#      {:ok, {scan4, _digest4}} = exec("worklog")
#      assert :ets.info(:ex_cache_ets, :size) == 4
#      assert scan4.numdocs == 7
#    end
#  end

#  describe "error condition" do
#    test "missing cfg" do
#      {status, msgs} = exec("unknown")
#      assert status == :error
#      assert msgs   == ["Config does not exist (unknown)"]
#    end
#  end

  # remove all cached products...
#  defp cleanup(_) do
#    Searchex.Command.clean
#    :ok
#  end
end
