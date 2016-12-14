defmodule Searchex.Command.CatalogTest do
  use ExUnit.Case

  import Searchex.Command.Catalog

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

  describe "error condition" do
    test "missing catalog cfg" do
      frame = exec("unknown")
      assert frame.halted
    end
  end

  describe "multiple runs" do
    test "doc count" do
      _frame1 = exec("min")
      assert :ets.info(:ex_cache_ets, :size) == 1
      _frame2 = exec("multi")
      assert :ets.info(:ex_cache_ets, :size) == 2
      _frame3 = exec("tweets")
      assert :ets.info(:ex_cache_ets, :size) == 3
      frame4 = exec("worklog")
      assert :ets.info(:ex_cache_ets, :size) == 4
      assert frame4.catalog.numdocs == 7
    end
  end
end
