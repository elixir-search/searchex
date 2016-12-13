defmodule LruRegistry.ServerTest do

  use ExUnit.Case, async: true

  import LruRegistry.Server
  import LruRegistry.Supervisor, only: [srv_name: 1]

  describe "server #start_link" do
    test "using default server name" do
      {:ok, pid} = start_link
      assert is_pid(pid)
    end

    test "no retained state" do
      assert Process.whereis(:server) == nil
    end

    test "using specific name and pool" do
      {:ok, pid} = start_link name: :tst_name, pool_size: 30
      assert is_pid(pid)
      assert srv_name(:tst_name)        == :lru_srv_tst_name
      assert Process.whereis(srv_name(:tst_name)) == pid
    end
  end

  describe "working with a server" do
    test "adding and removing processes" do
      start_link(name: :tst_server)
      start_link(name: :my_proc)
      assert num_procs(:tst_server)          == 0
      assert add(:tst_server, :my_proc)      == :ok
      assert num_procs(:tst_server)          == 1
      assert Process.whereis(srv_name(:my_proc))       != nil
      assert present?(:tst_server, :my_proc) == true
      assert touch(:tst_server, :my_proc)    == :ok
      assert delete(:tst_server, :my_proc)   == :ok
      assert Process.whereis(:my_proc)       == nil
      assert absent?(:tst_server, :my_proc)  == true
      assert num_procs(:tst_server)          == 0
    end
  end

end
