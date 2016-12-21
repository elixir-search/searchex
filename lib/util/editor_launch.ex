defmodule Util.EditorLaunch do
  @moduledoc false
  # Launch editors (like Vim, Emacs) from Elixir

  # Launching editors from Elixir is difficult. In other languages like Ruby, you
  # can invoke `system("vim")` and you're done.  But the BEAM hides the `tty` in
  # such a way that editors will not launch properly, so you have to use hacks
  # and workarounds like those provided in this module.

  # See this
  # [thread](https://elixirforum.com/t/how-to-launch-an-editor-from-escript/2094)
  # for more discussion.

  @doc """
  Launch editor on command-line console using TMUX

  The editor opens in a split window.  When the editor is closed, the
  split-window disappears.
  """
  def launch_using_tmux(file_path, line \\ 0) do
    cond do
      missing_editor?()         -> {:error, missing_editor_msg()}
      connected_without_tmux?() -> {:error, connected_without_tmux_msg(file_path)}
      true                      -> tmux_launch(file_path, line)
    end
  end

  # -----

  defp tmux_launch(file_path, line) do
    System.cmd "tmux", ["split-window", "vim", file_path, "+#{line}"]
    {:ok}
  end

  # -----

  def editor(), do: System.get_env("EDITOR")

  # -----

  def connected_without_tmux_msg(file_path), do:
  "To edit, run `#{editor()} #{file_path}`\nTIP: use TMUX to auto-launch the editor."

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
