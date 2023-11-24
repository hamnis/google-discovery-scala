#!/bin/bash

pid=$(cat server.pid)
if [ "Z${pid}" != "Z" ]; then
  echo $pid

  kill -9 $pid
  rm server.pid
fi