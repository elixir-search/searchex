defmodule Util.CacheTest do
  use ExUnit.Case

  describe "starting and stopping the cache" do
    test "before start" do
      assert Process.whereis(:ets_tst) == nil
    end

    test "after start" do
      Util.Cache.start(Searchex.Request.Params.exec("min"))
      assert Process.whereis(:ets_local_min) != nil
      assert Process.whereis(:dets_local_min) == nil
    end

    test "inter-test process persistence" do
      assert Process.whereis(:ets_local_min) == nil
    end

    test "up and down" do
      frame = Searchex.Request.Params.exec("min")
      Util.Cache.start(frame)
      refute Process.whereis(:ets_local_min) == nil
      Util.Cache.stop(frame)
      assert Process.whereis(:ets_local_min) == nil
    end
  end

  describe "getting and putting values" do
    test "basic ops" do
      frame = Searchex.Request.Params.exec("min")
      Util.Cache.start(frame)
      Util.Cache.put_cache(frame, "mykey", "myval")
      assert Util.Cache.get_cache(frame, "mykey") == "myval"
    end

    test "returns the cache key on put" do
      frame = Searchex.Request.Params.exec("min")
      Util.Cache.start(frame)
      assert Util.Cache.put_cache(frame, "mykey", "myval") == "mykey"
    end
  end

  describe "disk backup" do
    test "starting with path" do
      File.rm_rf("/tmp/searchex")
      assert File.dir?("/tmp/searchex") == false
      Util.Cache.start Searchex.Request.Params.exec("min")
      assert Process.whereis(:ets_local_min)  != nil
      assert Process.whereis(:dets_local_min) == nil
    end

    test "saving some data" do
      File.rm_rf("/tmp/searchex")
      frame = Searchex.Request.Params.exec("min")
      Util.Cache.start(frame)
      Util.Cache.put_cache(frame, "mykey", "myval")
      Util.Cache.save(frame)
      Util.Cache.stop(frame)
      Util.Cache.start(frame)
      assert Util.Cache.get_cache(frame, "mykey") == "myval"
    end
  end
end
