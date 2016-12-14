defmodule X.CacheTest do
  use ExUnit.Case
  
  describe "starting the cache" do
    test "before start" do
      assert Process.whereis(:ex_cache_ets) == nil
    end

    test "after start" do
      X.Cache.start
      assert Process.whereis(:ex_cache_ets) != nil
    end

    test "inter-test process persistence" do
      assert Process.whereis(:ex_cache_ets) == nil
    end
  end

  describe "getting and saving values" do
    test "basic ops" do
      X.Cache.start
      X.Cache.put_cache("mykey", "myval")
      assert X.Cache.get_cache("mykey") == "myval"
    end

    test "returns the cache key on put" do
      X.Cache.start
      assert X.Cache.put_cache("mykey", "myval") == "mykey"
    end
  end
end
