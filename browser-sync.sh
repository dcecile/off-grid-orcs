#!/bin/bash
trap "trap - SIGTERM && kill -- -$$" SIGINT SIGTERM EXIT
browser-sync start --server --files './*.html' 'target/scala-2.12/*.js' &
sbt
