#! /bin/bash

for file in "$@"
do
nohup xdg-open "$file" > ~/nohup.out &
done
sleep 1
