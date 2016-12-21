defmodule Searchex.Command.Info do

  @moduledoc false

  use Shake.Module

  @doc "Module API"
  def exec(cfg_snip) do
    call(%Frame{cfg_snip: cfg_snip}, [])
  end

  step Searchex.Command.Index

end