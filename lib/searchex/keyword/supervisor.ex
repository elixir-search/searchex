defmodule Searchex.Keyword.Supervisor do

  @moduledoc false

  use Supervisor

  @doc """
  Keyword Supervisor

  The keyword process tree is used to manage an inverted index for full-text
  searching.  The application has a single supervisor `Searchex.Keyword.Supervisor` and
  a one worker process for each keyword.
  """
  def start_link(pt_name) do
    Supervisor.start_link(__MODULE__, [], name: Util.Ext.Term.to_atom(pt_name))
  end

  @doc """
  Add KeywordSer worker process

  Use this function to add a new worker process for a keyword.  An error tuple
  is returned if the child already exists.
  """
  def add_child(pt_name, name) do
    Supervisor.start_child(Util.Ext.Term.to_atom(pt_name), worker(Searchex.Keyword.Server, [name], id: name))
  end

  @doc """
  Creates a child process, and returns the pid.
  """
  def add_child_and_return_pid(pt_name, name) do
    case add_child(Util.Ext.Term.to_atom(pt_name), name) do
      {:ok, pid}            -> pid
      {:error, {_msg, pid}} -> pid
      alt                   -> Util.Ext.IO.inspect(PODNAME: alt) ; nil
    end
  end

  def otp_to_map(pt_name) do
    list = Supervisor.which_children(Util.Ext.Term.to_atom(pt_name))
    Enum.reduce list, %{}, fn({child, _, _, _}, acc) ->
      vals = GenServer.call(child, :get_ids)
      Map.merge acc, %{child => vals}
    end
  end

  def map_to_otp(map, pt_name) do
    remove_all_otp_children(Util.Ext.Term.to_atom(pt_name))
    Map.keys(map)
    |> Enum.each(fn(key) ->
          srv = Searchex.Keyword.Server.get_keyword_server(pt_name, key)
          Searchex.Keyword.Server.set_state srv, map[key]
       end)
  end

  def remove_all_otp_children(pt_name) do
    list = Supervisor.which_children(pt_name)
    Enum.each list, fn({child, _, _, _}) ->
      Supervisor.delete_child(pt_name, child)
    end
  end

  def init([]) do
    supervise([], strategy: :one_for_one, restart: :transient)
  end
end
