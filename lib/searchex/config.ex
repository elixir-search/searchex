defmodule Searchex.Config do

  @moduledoc """
  Manage Searchex configuration files
  
  Configurations are stored in yaml files under `~/.searchex/<repo>/<cfg_name>.yml`.

      ---
      :collection:     "test"                          # must match file cfg_name
      :file_roots:     ["file.txt", "/home/user/sub"]  # a list of dirs and/or files
      :file_types:     ["txt", "md", "js"]             # leave blank for 'all files'
      :file_maxnum:    100                             # max number of files to load
      :file_maxkb:     100                             # max size of files to read
      :file_skips:     ["^_", "deps", "doc"]           # directories to skip
      :file_depth:     4                               # max recursive file levels
      :cache_dir:      "/tmp/testcache"                # location of the cache file
      :input_fields:                                   # input field specification
        :from:
          :regex:      "from: (?<from>.*)"
          :default:    "NA"
        :date:
          :regex:      "date: (?<date>.*)"
          :default:    "NA"
      :display_fields: "i:date f:filename1 body"       # results display spec
      :docsep:         "NNNN"                          # document separator regex

  ### Notes

  1. The only fields that are required are `:collection:` and `:file_roots:`
  2. When starting, restrict the number and type of files for fast processing
  3. The `i:` prefix on a display field matches an input field.
  4. The `f:` prefix on a display field matches a display function.

  ### Display Functions

  - `f:filename1` - show the filename
  - `f:filename2` - show the last two elements of the filename path
  - `f:filename3` - show the last three elements of the filename path

  ### Document Fields

  Any document field may be displayed in the results list.
    
      docid     - a five-character digest of the document contents 
      catid     - sequential id for the document within the overall catalog 
      fileid    - sequential id for the document within the file
      filename  - the document filename
      startline - the startline for the doc within the file
      startbyte - the startbyte for the doc within the file
      doclength - the document length (in bytes)
      wordcount - the number of words in a doc
      body      - the document body
      score     - the query score

  ### Ingestion Speed

  Searchex builds a number of intermedia data structures (params, catalog,
  index, query, results) that are saved in an LRU cache with auto-expiring keys.
  The cache is saved to disk.  Operations running over a live cache will run up
  to 1000X faster than when building data structures from scratch. Depending on
  the collection size, initial document ingestion can take many minutes to run.
  Once the caches are built, all operations should run sub-second.
  """

  @doc "Create a new config"
  def new(path) do
    Searchex.Config.New.exec(path)
  end

  @doc "Fetch a config from elixir-search/sample_docs"
  def fetch(repo) do
    Searchex.Config.Fetch.exec(repo)
  end

  @doc "Return the contents of a config"
  def cat(cfg_snip) do
    Searchex.Config.Cat.exec(cfg_snip)
  end

  @doc "Return information needed to edit the config"
  def edit(cfg_snip) do
    Searchex.Config.Edit.exec(cfg_snip)
  end

  @doc "Remove a config"
  def rm(cfg_snip) do
    Searchex.Config.Rm.exec(cfg_snip)
  end

  @doc "List the configs"
  def ls do
    Searchex.Config.Ls.exec
  end
end
