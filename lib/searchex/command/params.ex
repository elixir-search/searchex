defmodule Searchex.Command.Params do

  @moduledoc false

  alias Searchex.Command.CmdValidations
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
  step :validate_matching_cfg_names
  step Searchex.Command.Docsource
  step :generate_digest

  def generate_cfg_name(frame, _opts) do
    cfg_name = frame.cfg_snip |> Searchex.Config.CfgHelpers.cfg_name
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

  def validate_matching_cfg_names(frame, _opts) do
#    alias Searchex.Command.CmdHelpers
    frame_name = String.split(frame.cfg_name, "/") |> Enum.at(1)
    coll_name  = frame.params.collection
    case frame_name == coll_name do
      true -> frame
      _    -> halt(frame, "Mismatched collection name ('#{frame_name}' vs '#{coll_name}')")
    end
  end

  # generate a digest for the params
  # the digest is simply the newest timestamp of the config file
  # and all the documents in the file_roots

  # TODO: USE THE CURSOR FROM THE ADAPTER!!
  # ALSO: SEPARATE DIGESTS FOR DOCSOURCE AND PARAMS!!
  def generate_digest(%Frame{cfg_name: cfg_name} = frame, _opts) do
#    alias Searchex.Config.CfgHelpers
#    alias Searchex.Command.CmdHelpers
#    alias Util.TimeStamp
#    term  = [CfgHelpers.cfg_file(cfg_name)] ++ CmdHelpers.file_list(frame)
#            |> Enum.map(fn(file) -> TimeStamp.filepath_timestamp(file) end)
#            |> TimeStamp.newest
    term = "HELLO WORLD"
    digest = Util.Ext.Term.digest({cfg_name, term})
    set_digest(frame, :params, digest)
  end
end
