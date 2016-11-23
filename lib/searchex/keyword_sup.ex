defmodule Searchex.KeywordSup do

  @moduledoc false

  alias Searchex.Util.IO, as: DIO

  use Supervisor

  @doc """
  Start KeywordSup

  The keyword process tree is used to manage an inverted index for full-text
  searching.  The application has a single supervisor `Searchex.KeywordSup` and
  a one worker process for each keyword.
  """
  def start_link do
    Supervisor.start_link(__MODULE__, [], name: __MODULE__)
  end

  @doc """
  Add KeywordSer worker process

  Use this function to add a new worker process for a keyword.  An error tuple
  is returned if the child already exists.
  """
  def add_child(name) do
    Supervisor.start_child(__MODULE__, worker(Searchex.KeywordSer, [name], id: name))
  end

  @doc """
  Creates a child process, and returns the pid.
  """
  def add_child_and_return_pid(name) do
    case add_child(name) do
      {:ok, pid}            -> pid
      {:error, {_msg, pid}} -> pid
      alt                   -> DIO.inspect(PODNAME: alt) ; nil
    end
  end

  def init([]) do
    supervise([], strategy: :one_for_one, restart: :transient)
  end
end
