defmodule Searchex.Request.Info do

  @moduledoc false

  use Shreq.Module

  @doc "Module API"
  def exec(cfg_snip) do
    call(%Frame{cfg_snip: cfg_snip}, [])
  end

  step Searchex.Request.Index

end