defmodule Searchex.Config.Validations do

  import Shake.Frame
  import Searchex.Config.HelpersOld

  @moduledoc false

  def always_true(frame, _opts) do
    frame
  end

  def always_false(frame, _opts) do
    halt(frame, "ALWAYS FALSE VALIDATION")
  end

  def cfg_dir_absent?(frame, _opts) do
    path = cfg_path(frame.cfg_name)
    err  = "Config dir does not exist (#{path})"
    if File.dir?(path), do: frame, else: halt(frame, err)
  end

  def cfg_present?(frame, _opts) do
    cfg_name = frame.cfg_name
    err = "Config already exists (#{cfg_name})"
    if File.exists?(cfg_file(cfg_name)), do: halt(frame, err), else: frame
  end

  def cfg_missing?(frame, _opts) do
    cfg_name = frame.cfg_name
    err = "Config does not exist (#{cfg_name})"
    if File.exists?(cfg_file(cfg_name)), do: frame, else: halt(frame, err)
  end

  def cfg_name_invalid?(frame, _opts) do
    cfg_name = frame.cfg_name
    test = Regex.match?(~r/[^0-9A-Za-z-_]/, cfg_name)
    err  = "Invalid config name (#{cfg_name})"
    if test, do: halt(frame, err), else: frame
  end
end
