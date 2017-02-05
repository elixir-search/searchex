defmodule Searchex.Request.Build do

  @moduledoc false

  use Reqm.Module

  def exec(cfg_snip) do
    call(%Frame{cfg_snip: cfg_snip}, [])
  end

  step Searchex.Request.Index

end
