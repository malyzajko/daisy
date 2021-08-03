#!/bin/bash --posix

sbt compile

#generate daisy script
if [ ! -e daisy ]
then
  sbt script
fi