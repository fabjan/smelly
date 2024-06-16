#! /bin/sh

set -e

if ! command -v wrk > /dev/null
then
    echo "wrk not found, please install it"
    exit 1
fi

testbin=_build/example
testout=test.output

if [ ! -f $testbin ]
then
    echo "Test binary not found, please run build.sh first"
    exit 1
fi

# Start the test server
($testbin > $testout 2>&1) &
pid=$!

# Wait for it to start
while ! grep -q "Listening on" $testout
do
    sleep 1
done

# Run the test
wrk -d2 http://localhost:3000 > wrk.output

# Clean up
kill $pid

# Check the output
if ! grep -q "ERROR" $testout
then
    echo "Test passed"
else
    echo "Test failed"
    echo "Output:"
    cat $testout
    exit 1
fi
