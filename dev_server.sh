#!/bin/sh

sigint_handler()
{
  kill $PID
  exit
}

trap sigint_handler SIGINT

while true; do
  $@ &
  PID=$!
  fswatch -1 -e "node_modules" -e ".git" -e "dist" -e "dist-newstyle" `pwd`
  kill $PID
done

