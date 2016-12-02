defmodule Searchex.Keyword.SupervisorTest do

  use ExUnit.Case, async: true

  import Searchex.Keyword.Supervisor

  describe "#start_link" do
    test "using default name" do
      start_supervisor(:init)
      pid = Process.whereis(Searchex.Keyword.Supervisor)
      assert is_pid(pid)
      assert Process.whereis(Searchex.Keyword.Supervisor)  == pid
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
      assert Supervisor.count_children(Searchex.Keyword.Supervisor).active == 1
    end

    test "with two children" do
      add_child :asdf
      add_child :qwer
      assert is_pid(Process.whereis(:asdf))
      assert is_pid(Process.whereis(:qwer))
      assert Supervisor.count_children(Searchex.Keyword.Supervisor).active == 2
    end
  end

  defp start_supervisor(_) do
    case Searchex.Keyword.Supervisor.start_link do
      {:ok, pid}      -> pid
      {:error, _elem} ->
        start_supervisor(:restart)
    end
    :ok
  end
end
