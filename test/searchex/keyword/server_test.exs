#defmodule Searchex.Keyword.ServerTest do
#
#  use ExUnit.Case, async: true
#
#  import Searchex.Keyword.Server
#
#  describe "#start_link" do
#    test "using default name" do
#      pid = case start_link do
#        {:ok, pid} -> pid
#        _          -> :error
#      end
#      assert is_pid(pid)
#      assert Process.whereis(:server) == pid
#    end
#
#    test "no retained state" do
#      assert Process.whereis(:server) == nil
#    end
#
#    test "using specific name" do
#      {:ok, pid} = start_link :tst_name
#      assert is_pid(pid)
#      assert Process.whereis(:tst_name) == pid
#    end
#  end
#
#  describe "#get_keyword_server return value" do
#    setup :start_supervisor
#
#    test "with one word" do
#      assert is_pid(get_keyword_server(:test, "first"))
#    end
#
#    test "with repeat words" do
#      assert is_pid(get_keyword_server(:test, "first"))
#    end
#  end
#
#  describe "#get_keyword_server process counts" do
#    setup :start_supervisor
#
#    test "with no words" do
#      assert Supervisor.count_children(:test).active == 0
#    end
#
#    test "with one word" do
#      get_keyword_server(:test, "first")
#      assert Supervisor.count_children(:test).active == 1
#    end
#
#    test "with two words" do
#      get_keyword_server(:test, "first")
#      get_keyword_server(:test, "second")
#      assert Supervisor.count_children(:test).active == 2
#    end
#
#    test "with a repeat word" do
#      get_keyword_server(:test, "first")
#      get_keyword_server(:test, "first")
#      assert Supervisor.count_children(:test).active == 1
#    end
#  end
#
#  describe "#add_keyword_position" do
#    setup :start_supervisor
#
#    test "with one word" do
#      add_keyword_position(:test, "word", 99, 22)
#      assert Supervisor.count_children(:test).active == 1
#    end
#
#    test "with words in one document" do
#      add_keyword_position(:test, "word", 99, 22)
#      add_keyword_position(:test, "word", 99, 33)
#      assert Supervisor.count_children(:test).active == 1
#    end
#
#    test "with different words in one document" do
#      add_keyword_position(:test, "word", 99, 22)
#      add_keyword_position(:test, "prole", 99, 33)
#      assert Supervisor.count_children(:test).active == 2
#    end
#
#    test "with words in two documents" do
#      add_keyword_position(:test, "word", 99, 22)
#      add_keyword_position(:test, "word", 44, 33)
#      assert Supervisor.count_children(:test).active == 1
#    end
#
#    test "with different words in two documents" do
#      add_keyword_position(:test, "word", 99, 22)
#      add_keyword_position(:test, "prole", 44, 33)
#      assert Supervisor.count_children(:test).active == 2
#    end
#  end
#
#  describe "#get_ids" do
#    setup :start_supervisor
#
#    test "with empty index" do
#      result = get_ids(:test, "word")
#      assert result == {"word", %{}}
#    end
#
#    test "with a match" do
#      add_keyword_position(:test, "word", 99, 22)
#      get_ids(:test, "word")
#      result = get_ids(:test, "word")
#      assert result == {"word", %{99 => [22]}}
#    end
#
#    test "with two matches" do
#      add_keyword_position(:test, "word", 99, 22)
#      add_keyword_position(:test, "word", 99, 33)
#      result = get_ids(:test, "word")
#      assert result == {"word", %{99 => [22, 33]}}
#    end
#
#    test "with two documents" do
#      add_keyword_position(:test, "word", 93, 22)
#      add_keyword_position(:test, "word", 99, 33)
#      result = get_ids(:test, "word")
#      assert result == {"word", %{93 => [22], 99 => [33]}}
#    end
#  end
#
#  # -----
#
#  defp start_supervisor(_) do
#    case Searchex.Keyword.Supervisor.start_link(:test) do
#      {:ok, pid}      -> pid
#      {:error, _elem} ->
#        start_supervisor(:test)
#    end
#    :ok
#  end
#end
