defmodule Searchex.Request.CatalogTest do
  use ExUnit.Case

  alias Searchex.Request.Catalog

  describe "min collection" do
    test "doc count" do
      frame = Catalog.exec("min")
      assert frame.catalog.numdocs == 10
    end
  end

  describe "multi collection" do
    test "doc count" do
      frame = Catalog.exec("multi")
      assert frame.catalog.numdocs == 21
    end
  end

  describe "tweets collection" do
    test "doc count" do
      frame = Catalog.exec("tweets")
      assert frame.catalog.numdocs == 10
    end
  end

  describe "worklog collection" do
    test "doc count" do
      frame = Catalog.exec("worklog")
      assert frame.catalog.numdocs == 8
    end
  end

  describe "error condition" do
    test "missing catalog cfg" do
      frame = Catalog.exec("unknown")
      assert frame.halted
    end
  end

  describe "multiple runs" do
    test "doc count" do
      _frame1 = Catalog.exec("min")
      _frame2 = Catalog.exec("multi")
      _frame3 = Catalog.exec("tweets")
      frame4  = Catalog.exec("worklog")
      assert frame4.catalog.numdocs == 8
    end
  end
end
