use ExGuard.Config

guard("unit-test", run_on_start: true)
|> command("mix test --color")
|> watch({~r{lib/(?<dir>.+)/(?<file>.+).ex$}, fn(m) -> "test/#{m["dir"]}/#{m["file"]}_test.exs" end})
|> watch(~r{\.(erl|ex|exs|eex|xrl|yrl)\z}i)
