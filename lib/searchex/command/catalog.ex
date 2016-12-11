defmodule Searchex.Command.Catalog do


  @moduledoc false

#  import Searchex.Config.Helpers
#  import X.TimeStamp
  alias Shake.Frame
  use Shake.Module

  def exec(cfg_name) do
    call(%Frame{cfg_name: cfg_name}, [])
  end

  step Searchex.Command.Params
  step :generate

  def generate(frame, _opts) do
    catalog = frame.params
              |> Searchex.Command.Build.Catalog.create_from_params
    %Frame{frame | catalog: catalog}
  end

  # This is non-standard implementation of 'chain-children' at this level, the
  # two dependencies are the cfg file and the document files.  So instead of
  # calling to chained-children, we simply return the newest modification date.
#  def chain_children({:load_catalog, cfg_name}) do
#    params = gen_params(cfg_name)
#    term = [
#      filepath_timestamp(cfg_file(cfg_name))  , # timestamp of the cfg file
#      dirlist_timestamp(params.doc_dirs)        # newest timestamp of all doc_dirs
#    ] |> newest
#    [{:ok, term_digest(term, cfg_name)}]
#  end

#  def chain_generate({:load_catalog, cfg_name}, _child_state) do
#    gen_params(cfg_name)
#    |> Searchex.Command.Build.Catalog.create_from_params
#  end

end
