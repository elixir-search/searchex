defmodule Util.CacheTest do
  use ExUnit.Case
  
  describe "starting and stopping the cache" do
    test "before start" do
      assert Process.whereis(:ex_cache_ets) == nil
    end

    test "after start" do
      Util.Cache.start
      assert Process.whereis(:ex_cache_ets) != nil
      assert Process.whereis(:ex_cache_dets) == nil
    end

    test "inter-test process persistence" do
      assert Process.whereis(:ex_cache_ets) == nil
    end

    test "up and down" do
      assert Process.whereis(:ex_cache_ets) == nil
      Util.Cache.start
      refute Process.whereis(:ex_cache_ets) == nil
      Util.Cache.stop
      assert Process.whereis(:ex_cache_ets) == nil
    end
  end

  describe "getting and putting values" do
    test "basic ops" do
      Util.Cache.start
      Util.Cache.put_cache("mykey", "myval")
      assert Util.Cache.get_cache("mykey") == "myval"
    end

    test "returns the cache key on put" do
      Util.Cache.start
      assert Util.Cache.put_cache("mykey", "myval") == "mykey"
    end
  end

  describe "disk backup" do
    test "starting with path" do
      System.cmd "rm", ["-f", "/tmp/*dets"]
      Util.Cache.start path: "/tmp/my_cache.dets"
      assert Process.whereis(:ex_cache_ets)  != nil
      assert Process.whereis(:ex_cache_dets) == nil
    end

    test "saving some data" do
      System.cmd "rm", ["-f", "/tmp/*dets"]
      Util.Cache.start path: "/tmp/test1.dets"
      Util.Cache.put_cache("mykey", "myval")
      Util.Cache.save
      Util.Cache.stop
      Util.Cache.start path: "/tmp/test1.dets"
      assert Util.Cache.get_cache("mykey") == "myval"
    end
  end
end
