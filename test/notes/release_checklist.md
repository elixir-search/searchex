# Release Checklist

0. Rebuild Demo (optional)
   - update readme link
   - update website link 
1. Update Changelog
2. Update README
   - roadmap
   - install version
   - demo link (optional)
3. Update mix.exs
4. `mix test`
5. `test/bats/run`
8. `g c -am'Msg'; g push`
6. `g tag -a v0.0.x -m"v0.0.x"`
7. `g push origin v0.0.x`
9. mix hex.publish

