defmodule Shake do
  @type opts :: binary | tuple | atom | integer | float | [opts] | %{opts => opts}

  @callback init(opts) :: opts
  @callback call(Shake.Job.t, opts) :: Shake.Job.t
end
