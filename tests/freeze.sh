#!/bin/sh

if [ -z $1 ]
then
    echo "specify a test directory"
    exit 1
fi

FILES=$(find $1 -type f -not -name '*.lit' -not -name '*.html' -not -name '*.expected' -not -name '*build.sh' -not -name '*.gitignore')

for FILE in $FILES
do
    echo "freezing $FILE"
    cp $FILE $FILE.expected
done

