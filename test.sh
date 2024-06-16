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

test_failed=no

# Run the test
wrk -d2 'http://localhost:3000' > wrk.output

curlcheck()
{
    url=$1
    expected=$2
    actual=$(curl -s "$url")
    if ! echo "$actual" | grep -q "$expected"
    then
        echo "Expected output not found in server response: $url"
        echo "Expected: $expected"
        echo "Actual: $actual"
        test_failed=yes
    fi
}

curlcheck 'http://localhost:3000' 'Hello, world!'
curlcheck 'http://localhost:3000/goober' 'Hello, goober!'
curlcheck 'http://localhost:3000/foo?style=quiet' 'hi foo'
curlcheck 'http://localhost:3000/bar?style=loud' 'HELLO, BAR!!!'
curlcheck 'http://localhost:3000/baz?style=nope' 'Bad style'

# Clean up
kill $pid

# Check the output
if grep -q "ERROR" $testout
then
    echo "Error log in server output"
    test_failed=yes
fi

if [ $test_failed = yes ]
then
    echo "Test failed"
    #echo "Server output:"
    #cat $testout
    exit 1
fi

echo "Test passed"
