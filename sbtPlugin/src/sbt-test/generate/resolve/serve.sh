#!/bin/bash

nohup python3 -m http.server --directory ./discovery 25609 >>server.log 2>&1 &
FOO_PID=$!

if [ "$Z${FOO_PID}" != "Z" ]; then
  echo $FOO_PID > server.pid
  ./wait-for-port 25609
fi