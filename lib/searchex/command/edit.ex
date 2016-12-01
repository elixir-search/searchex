defmodule Searchex.Command.Edit do
#  use ExMake

  # error checks:
  # - valid cfg_name
  # - existing cfg_name
  # - valid cfg
  def exec(idnum) do
    results = elem(Searchex.Command.Search.Cache.read_results,0)
    docs    = results.docs
    doc     = Enum.at(docs, String.to_integer(idnum))
    cond do
      missing_editor?()           -> {:error, missing_editor_msg()}
      connected_without_tmux?()   -> {:error, connected_without_tmux_msg(doc)}
      true                        -> edit_doc(doc)
    end
  end

  def handle_chain(_cfg_name) do
    "TBD"
  end

  # -----

      defp edit_doc(doc_params) do
        System.cmd "tmux", ["split-window", "vim", doc_file(doc_params)]
        {:ok}
      end



    defp connected_without_tmux_msg(doc_params), do:
    "Can't launch editor without TMUX.  Run `#{editor()} #{doc_file(doc_params)}`"

    defp missing_editor_msg(), do:
    "No EDITOR defined - put `export EDITOR=<editor>` in your `.bashrc`"

        defp doc_file(doc_params) do
          basename = doc_params.filename
          extname  = Path.expand(basename)
          #    docstart = doc_params.docstart
          #    active_dirs.cfgs <> "/" <> cfg_name <> ".yml"
          # TODO: make the editor start at the right line.
          # TODO: record line number in
          "#{extname}"
        end

        defp editor()  , do: System.get_env("EDITOR")

        # -----

        defp connected_using_tmux?()    , do: System.get_env("TMUX") != nil

        defp connected_without_tmux?()  , do: ! connected_using_tmux?

        defp has_editor?()              , do: editor() != nil

        defp missing_editor?()          , do: ! has_editor?()


end