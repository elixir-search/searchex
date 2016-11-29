defmodule EditorLaunch do
  @moduledoc """
  Launching editors on the BEAM is difficult.

  See this thread for more discussion.
  https://elixirforum.com/t/how-to-launch-an-editor-from-escript/2094

  Two launch techniques are supported:
  - TMUX - launch the editor in a separate TMUX pane
  - Terminal - launch the editor in a separate TERMINAL [UNDER CONSTRUCTION]
  """

  @doc """
  Launch editor on command-line console

  Options:

  - line_num => <integer> the line number to start, default 0
  - editor => [vim|emacs|nano|etc.] it not supplied, $EDITOR is used
  - launch_if_directory => [:yes|:no|true|false] launch if `file_path` is a directory?

  Error conditions:
  - {:error, "Editor not found"}
  - {:error, "File not found"}
  - {:error, "No TTY"}
  - {:error, "TMUX Not Installed"}
  - {:error, "Unsupported Platform"} - only runs on Linux and Mac
  """
  def exec(_file_path, _opts \\ []) do
    "OK"
  end
end