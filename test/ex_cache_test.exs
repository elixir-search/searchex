defmodule ExCacheTest do
  use ExUnit.Case
  
  describe "starting the cache" do
    test "before start" do
      assert Process.whereis(:ex_cache_ets) == nil
    end

    test "after start" do
      ExCache.start
      assert Process.whereis(:ex_cache_ets) != nil
    end

    test "inter-test process persistence" do
      assert Process.whereis(:ex_cache_ets) == nil
    end
  end

  describe "getting and saving values" do
    test "basic ops" do
      ExCache.start
      ExCache.put_cache("mykey", "myval")
      assert ExCache.get_cache("mykey") == "myval"
    end

    test "returns the cache key on put" do
      ExCache.start
      assert ExCache.put_cache("mykey", "myval") == "mykey"
    end
  end
end
