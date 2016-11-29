defmodule EditorLaunch do
  @moduledoc """
  Launching editors on the BEAM is difficult.

  See this thread for more discussion.
  https://elixirforum.com/t/how-to-launch-an-editor-from-escript/2094
  """

  @doc """
  Launch editor on command-line console using TMUX
  """
  def using_tmux(file_path, _opts \\ []) do
    cond do
      missing_editor?()         -> {:error, missing_editor_msg()}
      connected_without_tmux?() -> {:error, connected_without_tmux_msg(file_path)}
      true                      -> tmux_launch(file_path)
    end
  end

  # -----

  defp tmux_launch(file_path) do
    System.cmd "tmux", ["split-window", "vim", file_path]
    {:ok}
  end

  # -----

  def editor()  , do: System.get_env("EDITOR")

  # -----

  def connected_without_tmux_msg(file_path), do:
  "Can't launch editor without TMUX.  Run `#{editor()} #{file_path}`"

  def missing_editor_msg(), do:
  "No EDITOR defined - put `export EDITOR=<editor>` in your `.bashrc`"

  # -----

  def has_editor?()            , do: editor() != nil
  def missing_editor?()        , do: ! has_editor?()
  def connected_using_tmux?()  , do: System.get_env("TMUX") != nil
  def connected_without_tmux?(), do: ! connected_using_tmux?

  # TODO: build out error conditions
  #    - {:error, "File not found"}
  #    - {:error, "No TTY"}
  #    - {:error, "Unsupported Platform"} - only runs on Linux and Mac

  # TODO: support editor options
  #  - line_num => <integer> the line number to start, default 0
  #  - editor => [vim|emacs|nano|etc.] it not supplied, $EDITOR is used
  #  - launch_if_directory => [:yes|:no|true|false] launch if `file_path` is a directory?

end