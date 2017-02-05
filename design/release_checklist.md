# Release Checklist

|------------------------------|-------|-------|
| Action                       | Major | Minor |
|------------------------------|:-----:|:-----:|
| Rebuild Demo [1]             |   X   |       |
| Update README [2]            |   X   |   X   |
| Update Changelog             |   X   |   X   |
| Update mix.exs               |   X   |   X   |
| `mix test`                   |   X   |   X   |
| `test/bats/run`              |   X   |   X   |
| `g c -am'Msg'; g push`       |   X   |   X   |
| `g tag -a v0.0.x -m"v0.0.x"` |   X   |   X   |
| `g push origin v0.0.x`       |   X   |   X   |
| `mix hex.publish`            |   X   |   X   |
| Blog Post                    |   X   |       |
| Forum Announcement           |   X   |       |
| Tweet                        |   X   |       |
|------------------------------|-------|-------|

[1] Rebuild Demo
- update readme link
- update website link

[2] Update README
- roadmap
- install version
- demo link

