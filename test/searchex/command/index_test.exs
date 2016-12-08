defmodule Searchex.Command.IndexTest do
  use ExUnit.Case

#  import Searchex.Command.Index

  setup :cleanup

#  describe "min collection" do
#    test "doc count" do
#      {:ok, {scan, _digest}} = exec("min")
#      assert scan.numdocs == 0
#      # should be 10
#    end
#  end

#  describe "multi collection" do
#    test "doc count" do
#      {:ok, {scan, _digest}} = exec("multi")
#      assert scan.numdocs == 7
#    end
#  end
#
#  describe "tweets collection" do
#    test "doc count" do
#      {:ok, {scan, _digest}} = exec("tweets")
#      assert scan.numdocs == 0
#    end
#  end
#
#  describe "worklog collection" do
#    test "doc count" do
#      {:ok, {scan1, _digest1}} = exec("worklog")
#      assert scan1.numdocs == 7
#    end
#  end

#  describe "multiple runs" do
#    test "doc count" do
#      TIO.inspect "A"
#      {:ok, {_scan1, _digest1}} = exec("min")
#      TIO.inspect "B"
#      assert :ets.info(:ex_cache_ets, :size) == 2
#      TIO.inspect "C"
#      {:ok, {_scan2, _digest2}} = exec("multi")
#      TIO.inspect "D"
#      assert :ets.info(:ex_cache_ets, :size) == 2
#      TIO.inspect "E"
#      {:ok, {_scan3, _digest3}} = exec("tweets")
#      TIO.inspect "F"
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
  defp cleanup(_) do
    Searchex.Command.clean
    :ok
  end
end
