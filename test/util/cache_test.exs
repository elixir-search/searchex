defmodule Util.CacheTest do
  use ExUnit.Case
  
  describe "starting and stopping the cache" do
    test "before start" do
      assert Process.whereis(:ets_tst) == nil
    end

    test "after start" do
      Util.Cache.start("tst")
      assert Process.whereis(:ets_tst) != nil
      assert Process.whereis(:dets_tst) == nil
    end

    test "inter-test process persistence" do
      assert Process.whereis(:ets_tst) == nil
    end

    test "up and down" do
      assert Process.whereis(:ets_tst) == nil
      Util.Cache.start("tst")
      refute Process.whereis(:ets_tst) == nil
      Util.Cache.stop("tst")
      assert Process.whereis(:ets_tst) == nil
    end
  end

  describe "getting and putting values" do
    test "basic ops" do
      Util.Cache.start("tst")
      Util.Cache.put_cache("tst", "mykey", "myval")
      assert Util.Cache.get_cache("tst", "mykey") == "myval"
    end

    test "returns the cache key on put" do
      Util.Cache.start("tst")
      assert Util.Cache.put_cache("tst", "mykey", "myval") == "mykey"
    end
  end

  describe "disk backup" do
    test "starting with path" do
      System.cmd "rm", ["-f", "/tmp/*dets"]
      Util.Cache.start("tst")
      assert Process.whereis(:ets_tst)  != nil
      assert Process.whereis(:dets_tst) == nil
    end

    test "saving some data" do
      System.cmd "rm", ["-f", "/tmp/*dets"]
      Util.Cache.start"tst"
      Util.Cache.put_cache("tst", "mykey", "myval")
      Util.Cache.save("tst")
      Util.Cache.stop("tst")
      Util.Cache.start("tst")
      assert Util.Cache.get_cache("tst", "mykey") == "myval"
    end
  end
end
