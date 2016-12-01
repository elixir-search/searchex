defmodule Searchex.Config.Edit do

  @moduledoc false

  import Searchex.Config.Helpers

  def exec(cfg_name) do
    make_active_dirs()
    cond do
#      cfg_name_invalid?(cfg_name) -> {:error, cfg_name_invalid_msg(cfg_name)}
#      cfg_missing?(cfg_name)      -> {:error, cfg_missing_msg(cfg_name)}
      true                        -> {:ok, cfg_name}
    end
  end
end