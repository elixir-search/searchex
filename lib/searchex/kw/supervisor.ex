defmodule Searchex.Kw.Supervisor do

  @moduledoc false

  use Supervisor

  @doc """
  Keyword Supervisor

  The keyword process tree is used to manage an inverted index for full-text
  searching.  The application has a single supervisor `Searchex.Kw.Supervisor` and
  a one worker process for each keyword.
  """
  def start_link(collection) do
    Supervisor.start_link(__MODULE__, [], name: collection)
  end

  @doc """
  Add KeywordSer worker process

  Use this function to add a new worker process for a keyword.  An error tuple
  is returned if the child already exists.
  """
  def add_child(collection, name) do
    Supervisor.start_child(collection, worker(Searchex.Kw.Server, [name], id: name))
  end

  @doc """
  Creates a child process, and returns the pid.
  """
  def add_child_and_return_pid(collection, name) do
    case add_child(collection, name) do
      {:ok, pid}            -> pid
      {:error, {_msg, pid}} -> pid
      alt                   -> DIO.inspect(PODNAME: alt) ; nil
    end
  end

  def otp_to_term(col) do
#    list = Supervisor.which_children(Searchex.Kw.Supervisor)
    list = Supervisor.which_children(col)
    Enum.reduce list, %{}, fn({child, _, _, _}, acc) ->
      vals = GenServer.call(child, :get_ids)
      Map.merge acc, %{child => vals}
    end
  end

  def term_to_otp(col, map) do
    remove_all_otp_children(col)
    Map.keys(map)
    |> Enum.each(fn(key) ->
          srv = Searchex.Kw.Server.get_keyword_server(col, key)
          Searchex.Kw.Server.set_state srv, map[key]
       end)
  end

  defp remove_all_otp_children(col) do
#    list = Supervisor.which_children(Searchex.Kw.Supervisor)
    list = Supervisor.which_children(col)
    Enum.each list, fn({child, _, _, _}) ->
      Supervisor.delete_child(col, child)
    end
  end

  def init([]) do
    supervise([], strategy: :one_for_one, restart: :transient)
  end
end
