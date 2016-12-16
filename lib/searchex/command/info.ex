defmodule Searchex.Command.Info do

  @moduledoc false

  use Shake.Module

  @doc "Module API"
  def exec(cfg_name) do
    call(%Frame{cfg_name: cfg_name}, [])
  end

  step Searchex.Command.Index

end