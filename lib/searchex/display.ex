defmodule Searchex.Display do
  @moduledoc """
  Utility methods for Searchex display

  - table rendering
  - editor launch
  - etc.
  """

  @doc "Render a table"
  def render_table(args) do
    "TBD"
  end

  @doc "Launch an editor"
  def editor_launch(file_path, args \\ []) do
    EditorLaunch.exec(file_path, args)
  end
end