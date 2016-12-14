defmodule LruRegistry.SupervisorTest do

  use ExUnit.Case, async: true

  import LruRegistry.Supervisor

  describe "#start_link" do
    test "using test name" do
      name = "test"
      {:ok, pid} = start_link(LruRegistry.Supervisor, [name: name], [])
      assert is_pid(pid)
      assert Process.whereis(sup_name(name))  == pid
      assert Supervisor.count_children(pid).active == 1
      assert Process.whereis(srv_name(name))  != nil
    end
  end
end