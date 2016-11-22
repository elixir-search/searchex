defmodule Searchex.KeywordSupTest do

  use ExUnit.Case, async: true

  import Searchex.KeywordSup

  describe "#start_link" do
    test "using default name" do
      start_supervisor(:init)
      pid = Process.whereis(Searchex.KeywordSup)
      assert is_pid(pid)
      assert Process.whereis(Searchex.KeywordSup)  == pid
      assert Supervisor.count_children(pid).active == 0
    end

    test "has no retained state" do
      assert Process.whereis(:server) == nil
    end
  end

  describe "#add_child" do
    setup :start_supervisor

    test "with one child" do
      add_child :asdf
      assert is_pid(Process.whereis(:asdf))
      assert Supervisor.count_children(Searchex.KeywordSup).active == 1
    end

    test "with two children" do
      add_child :asdf
      add_child :qwer
      assert is_pid(Process.whereis(:asdf))
      assert is_pid(Process.whereis(:qwer))
      assert Supervisor.count_children(Searchex.KeywordSup).active == 2
    end
  end

  defp start_supervisor(_) do
    case Searchex.KeywordSup.start_link do
      {:ok, pid}      -> pid
      {:error, _elem} ->
        #IO.inspect(START_SUP_ERROR_B: elem)
        start_supervisor(:restart)
    end
    :ok
  end
end
