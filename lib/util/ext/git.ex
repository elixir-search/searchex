defmodule Util.Ext.Git do

  @moduledoc false

  def is_repo?(repo_dir) do
    File.exists?(repo_dir <> "/.git")
  end

  def init_repo(repo_dir) do
    unless is_repo?(repo_dir) do
      System.cmd("git", ["init"], cd: repo_dir)
    end
  end

  def has_changes?(repo_dir) do
    System.cmd("git", ["status", "-s"], cd: repo_dir)
    |> elem(0)
    |> Util.Ext.String.present?
  end

  def auto_commit(repo_dir) do
    unless has_changes?(repo_dir) do
      System.cmd("git", ["add"   , "."]                        , cd: repo_dir)
      System.cmd("git", ["commit", "-am'Update #{time_now()}'"], cd: repo_dir)
    end
  end

  def head_id(repo_dir) do
    Enum.at(commits(repo_dir), -1)
  end

  def commits_after(repo_dir, target) do
    {:ok, reg} = Regex.compile(target)
    list       = commits(repo_dir)
    idx        = Enum.find_index(list, fn(el) -> Regex.match?(reg, el) end) + 1
    case idx do
      nil -> nil
      _   -> Enum.split(list, idx) |> elem(1)
    end
  end

  def commits(repo_dir) do
    System.cmd("git", ["log", "--format='%h'"], cd: repo_dir)
    |> elem(0)
    |> String.replace("'", "")
    |> String.split("\n")
    |> Enum.filter(fn(el) -> Util.Ext.String.present?(el) end)
    |> Enum.reverse
    |> Enum.with_index
    |> Enum.map(fn({str, num}) -> "#{num}_#{str}" end)
  end

  def events(repo_dir, target) do
    cid = commit_id(head_id(repo_dir))
    tid = commit_id(target)
    System.cmd("git", ["diff-tree", "--name-status", "--no-commit-id", "-r", cid, tid], cd: repo_dir)
    |> elem(0)
    |> String.split("\n")
    |> Enum.filter(fn(el) -> Util.Ext.String.present?(el) end)
    |> Enum.map(fn(el)    -> String.split(el, "\t") end)
    |> Enum.map(fn([lbl, path]) -> {action(lbl), path} end)
  end

  # -----

  defp action(lbl) do
    case lbl do
      "M" -> :update
      "A" -> :change
      "D" -> :delete
    end
  end

  defp commit_id(str) do
    str |> String.reverse |> String.split("_") |> List.first |> String.reverse
  end


  defp time_now do
    System.cmd("date", [])
    |> elem(0)
    |> String.trim
  end
end
