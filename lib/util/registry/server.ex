defmodule Util.Registry.Server do

  @moduledoc false

  use GenServer

  import Util.Registry.Supervisor, only: [srv_name: 1]

  @doc """
  Util.Registry Server
  """
  def start_link(opts1 \\ []) do
    opts2 = Keyword.merge([name: :lru_registry_server, pool_size: 20], List.flatten(opts1))
    GenServer.start_link(__MODULE__, {[], opts2[:pool_size]}, name: srv_name(opts2[:name]))
  end

  # --- API
  
  def add(registry_name, process_name) do
    GenServer.call(srv_name(registry_name), {:add, process_name})
  end

  def delete(registry_name, process_name) do
    GenServer.call(srv_name(registry_name), {:delete, process_name})
  end

  def present?(registry_name, process_name) do
    GenServer.call(srv_name(registry_name), {:present?, process_name})
  end

  def absent?(registry_name, process_name) do
    GenServer.call(srv_name(registry_name), {:absent?, process_name})
  end

  def num_procs(registry_name) do
    GenServer.call(srv_name(registry_name), {:num_procs})
  end

  def get_pool_size(registry_name) do
    GenServer.call(srv_name(registry_name), {:get_pool})
  end

  def set_pool_size(registry_name, new_size) do
    GenServer.call(srv_name(registry_name), {:set_pool, new_size})
  end

  def touch(registry_name, process_name) do
    GenServer.cast(srv_name(registry_name), {:touch, process_name})
  end

  # --- CALLBACKS

  def init(args) do
    {:ok, args}
  end

  def handle_call({:add, process_name}, _, {list, pool_size}) do
    new_list = [process_name] ++ list
    alt_list = if length(new_list) > pool_size do
                 last = List.last(new_list)
                 if Process.whereis(last), do: GenServer.stop(last)
                 List.delete(new_list, last)
               else
                 new_list
               end
    {:reply, :ok, {alt_list, pool_size}}
  end

  def handle_call({:delete, process_name}, _, {list, pool_size}) do
    new_list = if Enum.member?(list, process_name) do
                 if Process.whereis(process_name), do: GenServer.stop(process_name)
                 List.delete(list, process_name)
               else
                 list
               end
    {:reply, :ok, {new_list, pool_size}}
  end

  def handle_call({:present?, process_name}, _, {list, pool_size}) do
    {:reply, Enum.member?(list, process_name), {list, pool_size}}
  end

  def handle_call({:absent?, process_name}, _, {list, pool_size}) do
    {:reply, ! Enum.member?(list, process_name), {list, pool_size}}
  end

  def handle_call({:num_procs}, _, {list, pool_size}) do
    {:reply, length(list), {list, pool_size}}
  end

  # TODO: delete processes if the pool shrinks
  def handle_call({:set_pool, new_pool_size}, _, {list, _pool_size}) do
    {:reply, :ok, {list, new_pool_size}}
  end

  def handle_call({:get_pool}, _, {list, pool_size}) do
    {:reply, pool_size, {list, pool_size}}
  end

  def handle_cast({:touch, process_name}, {list, pool_size}) do
    new_list = if Enum.member?(list, process_name) do
                 [process_name] ++ List.delete(list, process_name)
               else
                 list
               end
    {:noreply, {new_list, pool_size}}
  end
end
