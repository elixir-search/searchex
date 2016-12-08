defmodule Searchex.Kw.SupervisorTest do

  use ExUnit.Case, async: true

  import Searchex.Kw.Supervisor

  describe "#start_link" do
    test "using default namee" do
      start_supervisor(:test)
      pid = Process.whereis(:test)
      assert is_pid(pid)
      assert Process.whereis(:test)  == pid
      assert Supervisor.count_children(pid).active == 0
    end

    test "has no retained statee" do
      assert Process.whereis(:asdf) == nil
    end
  end

  describe "#add_childd" do
    test "with one child" do
      start_supervisor(:test)
      add_child :test, :asdf
      assert is_pid(Process.whereis(:test))
      assert Supervisor.count_children(:test).active == 1
    end

    test "with two childrenn" do
      start_supervisor(:test)
      add_child :test, :asdf
      add_child :test, :qwer
      assert is_pid(Process.whereis(:asdf))
      assert is_pid(Process.whereis(:qwer))
      assert Supervisor.count_children(:test).active == 2
    end
  end

  defp start_supervisor(_) do
    case Searchex.Kw.Supervisor.start_link(:test) do
      {:ok, pid}      -> pid
      {:error, _elem} ->
        start_supervisor(:test)
    end
    :ok
  end
end