defmodule Searchex.Command.Params do

  @moduledoc false

  alias Searchex.Config.CfgHelpers
  alias Searchex.Config.CmdValidations
  use Shake.Module

  @doc """
  The API for the module - takes a config name and returns
  a frame with the Params slot filled.
  """
  def exec(cfg_snip) do
    call(%Frame{cfg_snip: cfg_snip}, [])
  end

  validation_list = [
    &CmdValidations.cfg_nomatch?/2        ,
    &CmdValidations.cfg_ambiguous?/2      ,
  ]

  step :validate, with: [&CmdValidations.cfg_snip_invalid?/2]
  step :validate, with: validation_list
  step :generate_cfg_name
  step :generate_params
  step :validate_file_paths
  step :generate_digest
  step :start_cache

  def generate_cfg_name(frame, _opts) do
    cfg_name = frame.cfg_snip |> CfgHelpers.cfg_name
    %Frame{frame | cfg_name: cfg_name}
  end

  # read the config file and generate params
  def generate_params(frame, _opts) do
    params = frame.cfg_name
             |> Searchex.Config.cat
             |> Searchex.Config.Load.to_map
             |> Util.Ext.Map.atomify_keys
             |> Searchex.Command.Build.Catalog.Params.create_from_cfg
    %Frame{frame | params: params}
  end

  # halt if one or more of the doc dirs is missing
  def validate_file_paths(frame, _opts) do
    badpaths = frame.params.file_paths
               |> Enum.map(fn(path) -> {File.dir?(path), path} end)
               |> Enum.filter(fn(tup) -> ! elem(tup, 0) end)
               |> Enum.map(fn(tup) -> elem(tup, 1) end)
               |> Enum.join(", ")
    case badpaths do
      "" -> frame
      _  -> halt(frame, "Missing file path (#{badpaths})")
    end
  end

  # generate a digest for the params
  # the digest is simply the newest timestamp of the config file
  # and all the documents in the file_paths
  def generate_digest(%Frame{cfg_name: cfg_name, params: params} = frame, _opts) do
    import Searchex.Config.CfgHelpers
    import Util.TimeStamp
    term   = mixlist_timestamp([cfg_file(cfg_name), params.file_paths])
    digest = Util.Ext.Term.digest({cfg_name, term})
    set_digest(frame, :params, digest)
  end

  def start_cache(frame, _opts) do
    Util.Cache.start(frame.cfg_name)
    frame
  end
end
