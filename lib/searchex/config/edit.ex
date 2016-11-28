defmodule Searchex.Config.Edit do

  @moduledoc false

  import Searchex.Config.Helpers

  def exec(cfg_name) do
    make_active_dirs()
    cond do
      missing_editor?()           -> {:error, missing_editor_msg()}
      connected_without_tmux?()   -> {:error, connected_without_tmux_msg(cfg_name)}
      cfg_name_invalid?(cfg_name) -> {:error, cfg_name_invalid_msg(cfg_name)}
      cfg_missing?(cfg_name)      -> {:error, cfg_missing_msg(cfg_name)}
      true                        -> edit_cfg(cfg_name)
    end
  end

  defp edit_cfg(cfg_name) do
    System.cmd "tmux", ["split-window", "vim", cfg_file(cfg_name)]
    {:ok}
  end
end