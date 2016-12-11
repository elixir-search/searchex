defmodule Searchex.Command.Params do

  @moduledoc false

  alias Shake.Frame
  alias Searchex.Config.Validations, as: V
  import X.TimeStamp
  import X.Term
  import Searchex.Config.Helpers
  use Shake.Module

  def exec(cfg_name) do
    call(%Frame{cfg_name: cfg_name}, [])
  end

  validation_list = [
    &V.cfg_dir_absent?/2   ,
    &V.cfg_missing?/2      ,
  ]

  step :validate, with: validation_list
  step :generate_params
  step :generate_digest

  def generate_params(frame, _opts) do
    params = frame.cfg_name
             |> Searchex.Config.cfg_cat
             |> Searchex.Config.Load.to_map
             |> Searchex.Util.Map.atomify_keys
             |> Searchex.Command.Build.Catalog.Params.create_from_cfg
    %Frame{frame | params: params}
  end

  def generate_digest(%Frame{cfg_name: cfg_name, params: params} = frame, _opts) do
    term = [
      filepath_timestamp(cfg_file(cfg_name))  , # timestamp of the cfg file
      dirlist_timestamp(params.doc_dirs)        # newest timestamp of all doc_dirs
    ] |> newest
    digest = digest({term, cfg_name})
    set_digest(frame, :params, digest)
  end
end
