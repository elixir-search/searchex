defmodule Searchex.Build.Catalog.Scan.Doc do

  @moduledoc false

  defstruct filename:   ""   ,
            docid:      ""   ,
            docstart:   0    ,
            doclength:  0    ,
            wordcount:  0    ,
            wordstems:  []   ,
            fields:     %{}  , 
            body:       ""

end
