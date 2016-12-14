defmodule Searchex.Command.Params do

  @moduledoc false

  alias Searchex.Config.Validations
  use Shake.Module

  @doc """
  The API for the module - takes a config name and returns
  a frame with the Params slot filled.
  """
  def exec(cfg_name) do
    call(%Frame{cfg_name: cfg_name}, [])
  end

  validation_list = [
    &Validations.cfg_dir_absent?/2   ,
    &Validations.cfg_missing?/2      ,
  ]

  step :validate, with: validation_list
  step :generate_params
  step :validate_doc_dirs
  step :generate_digest

  # read the config file and generate params
  def generate_params(frame, _opts) do
    params = frame.cfg_name
             |> Searchex.Config.cfg_cat
             |> Searchex.Config.Load.to_map
             |> Util.Ext.Map.atomify_keys
             |> Searchex.Command.Build.Catalog.Params.create_from_cfg
    %Frame{frame | params: params}
  end

  # halt if one or more of the doc dirs is missing
  def validate_doc_dirs(frame, _opts) do
    badpaths = frame.params.doc_dirs
               |> Enum.map(fn(path) -> {File.dir?(path), path} end)
               |> Enum.filter(fn(tup) -> ! elem(tup, 0) end)
               |> Enum.map(fn(tup) -> elem(tup, 1) end)
               |> Enum.join(", ")
    case badpaths do
      "" -> frame
      _  -> halt(frame, "Missing doc dir (#{badpaths})")
    end
  end

  # generate a digest for the params
  # the digest is simply the newest timestamp of the config file
  # and all the documents in the doc_dirs
  def generate_digest(%Frame{cfg_name: cfg_name, params: params} = frame, _opts) do
    import Searchex.Config.Helpers
    import Util.TimeStamp
    term = [
      filepath_timestamp(cfg_file(cfg_name))  , # timestamp of the cfg file
      dirlist_timestamp(params.doc_dirs)        # newest timestamp of all doc_dirs
    ] |> newest
    digest = Util.Ext.Term.digest({cfg_name, term})
    set_digest(frame, :params, digest)
  end
end
