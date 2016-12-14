defmodule Util.Registry.Supervisor do

  @moduledoc false

  use Supervisor

  @doc """
  Util.Registry Server
  """
  def start_link(_, opts1, []) do
    opts2 = Keyword.merge([name: "default", pool_size: 20], opts1)
    Supervisor.start_link(__MODULE__, opts2, name: sup_name(opts2[:name]))
  end

  def init(opts) do
    processes = [worker(Util.Registry.Server, [opts])]
    supervise(processes, strategy: :one_for_one, restart: :transient)
  end

  def sup_name(base) do
    Util.Ext.Term.join_atoms ["lru_sup_", base]
  end

  def srv_name(base) do
    Util.Ext.Term.join_atoms ["lru_srv_", base]
  end
end
