defmodule Searchex.Kw.ServerTest do

  use ExUnit.Case, async: true

  import Searchex.Kw.Server

  describe "#start_link" do
    test "using default name" do
      pid = case start_link do
        {:ok, pid} -> pid
        _          -> :error
      end
      assert is_pid(pid)
      assert Process.whereis(:server) == pid
    end

    test "no retained state" do
      assert Process.whereis(:server) == nil
    end

    test "using specific name" do
      {:ok, pid} = start_link :tst_name
      assert is_pid(pid)
      assert Process.whereis(:tst_name) == pid
    end
  end

  describe "#get_keyword_server return value" do
    setup :start_supervisor

    test "with one word" do
      assert is_pid(get_keyword_server("first"))
    end

    test "with repeat words" do
      assert is_pid(get_keyword_server("first"))
    end
  end

  describe "#get_keyword_server process counts" do
    setup :start_supervisor

    test "with no words" do
      assert Supervisor.count_children(Searchex.Kw.Supervisor).active == 0
    end

    test "with one word" do
      get_keyword_server("first")
      assert Supervisor.count_children(Searchex.Kw.Supervisor).active == 1
    end

    test "with two words" do
      get_keyword_server("first")
      get_keyword_server("second")
      assert Supervisor.count_children(Searchex.Kw.Supervisor).active == 2
    end

    test "with a repeat word" do
      get_keyword_server("first")
      get_keyword_server("first")
      assert Supervisor.count_children(Searchex.Kw.Supervisor).active == 1
    end
  end
  
  describe "#add_keyword_position" do
    setup :start_supervisor

    test "with one word" do
      add_keyword_position("word", 99, 22)
      assert Supervisor.count_children(Searchex.Kw.Supervisor).active == 1
    end

    test "with words in one document" do
      add_keyword_position("word", 99, 22)
      add_keyword_position("word", 99, 33)
      assert Supervisor.count_children(Searchex.Kw.Supervisor).active == 1
    end

    test "with different words in one document" do
      add_keyword_position("word", 99, 22)
      add_keyword_position("prole", 99, 33)
      assert Supervisor.count_children(Searchex.Kw.Supervisor).active == 2
    end

    test "with words in two documents" do
      add_keyword_position("word", 99, 22)
      add_keyword_position("word", 44, 33)
      assert Supervisor.count_children(Searchex.Kw.Supervisor).active == 1
    end

    test "with different words in two documents" do
      add_keyword_position("word", 99, 22)
      add_keyword_position("prole", 44, 33)
      assert Supervisor.count_children(Searchex.Kw.Supervisor).active == 2
    end
  end

  describe "#get_ids" do
    setup :start_supervisor

    test "with empty index" do
      result = get_ids("word")
      assert result == {"word", %{}}
    end

    test "with a match" do
      add_keyword_position("word", 99, 22)
      get_ids("word")
      result = get_ids("word")
      assert result == {"word", %{99 => [22]}}
    end

    test "with two matches" do
      add_keyword_position("word", 99, 22)
      add_keyword_position("word", 99, 33)
      result = get_ids("word")
      assert result == {"word", %{99 => [22, 33]}}
    end

    test "with two documents" do
      add_keyword_position("word", 93, 22)
      add_keyword_position("word", 99, 33)
      result = get_ids("word")
      assert result == {"word", %{93 => [22], 99 => [33]}}
    end
  end

  # -----

  defp start_supervisor(_) do
    case Searchex.Kw.Supervisor.start_link do
      {:ok, pid}      -> pid
      {:error, _elem} ->
        start_supervisor(:restart)
    end
    :ok
  end
end
