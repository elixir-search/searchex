#!/usr/bin/env bats

@test "clean $COLL" {
  run searchex clean $COLL
  [ "$status" -eq 0 ]
}

@test "catalog $COLL" {
  run searchex catalog $COLL
  [ "$status" -eq 0 ]
}

@test "index $COLL" {
  run searchex index $COLL
  [ "$status" -eq 0 ]
}

@test "build $COLL" {
  run searchex build $COLL
  [ "$status" -eq 0 ]
}

@test "query $COLL" {
  run searchex query $COLL .
  [ "$status" -eq 0 ]
}

@test "results $COLL" {
  run searchex results $COLL
  [ "$status" -eq 0 ]
}

@test "show $COLL" {
  run searchex show $COLL 1
  [ "$status" -eq 0 ]
}
