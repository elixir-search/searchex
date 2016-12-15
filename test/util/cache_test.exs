defmodule Util.CacheTest do
  use ExUnit.Case
  
  describe "starting the cache" do
    test "before start" do
      assert Process.whereis(:ex_cache_ets) == nil
    end

    test "after start" do
      Util.Cache.start
      assert Process.whereis(:ex_cache_ets) != nil
    end

    test "inter-test process persistence" do
      assert Process.whereis(:ex_cache_ets) == nil
    end
  end

  describe "getting and saving values" do
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
end
