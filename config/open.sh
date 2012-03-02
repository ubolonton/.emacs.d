#! /bin/bash
# To open file natively from inside dired

for file in "$@"
do
nohup xdg-open "$file" > ~/nohup.out &
done
sleep 1
