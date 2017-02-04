defmodule Searchex.App do

  use Application

  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    Util.Ext.IO.tins("STARTING", color: "GREEN")

    children = [
      worker(Registry, [:unique, :SearchexRegistry] ),
      worker(Calc1,    []                           ),
      worker(Calc2,    []                           )
      # supervisor(Searchex.Keyword.Supervisor, []   )
    ]

    opts = [strategy: :one_for_one, name: :SearchexSupervisor]
    Supervisor.start_link(children, opts)
  end
end
