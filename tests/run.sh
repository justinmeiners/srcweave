#!/bin/sh

# To run tests, this package must be in your quicklisp local-projects.

TEST_FILES="$(find . -name '*build.sh')"

for TEST in $TEST_FILES
do
    (
    echo "TESTING: $TEST";
    cd "$(dirname $TEST)";
    sh "$(basename $TEST)";
    if [ $? -ne 0 ]
    then
        echo "*** FAILED. test script error. ***"
        echo ""
        continue
    fi

    ERROR=""
    EXPECTED_FILES="$(find . -name '*.expected')";
    for E in $EXPECTED_FILES
    do
        S="$(dirname $E)/$(basename $E ".expected")";

        echo "checking: $S"
        if [ -f $E ]
        then
            cmp "$S" "$E"
            if [ $? -ne 0 ]
            then
                ERROR="1"
                echo "*** FAILED. Didn't match expected ***"
                echo ""
                diff -u "$E" "$S"
                continue
            fi
        fi
    done;
    if [ -z "$ERROR" ]
    then
        echo "PASS"
    else
        echo "** FAIL **"
    fi
    echo ""
    )
done
