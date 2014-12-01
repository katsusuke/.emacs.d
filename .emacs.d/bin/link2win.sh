#!/bin/bash
for link_name in $(find . -type l); do
  link_path=`readlink $link_name`
  echo "$link_name"|sed 's%./%%g'|sed 's#^#del /A:S #g'
  echo "$link_name $link_path"|sed 's#/#\\#g'|sed 's%^%mklink /D %g'
done
