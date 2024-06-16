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

($testbin > $testout 2>&1) &
pid=$!

printf "Waiting for server to start "
while ! grep -q "Listening on" $testout
do
    sleep 1
    printf "."
done
printf " ready\n"

test_failed=no

curlcheck()
{
    url=$1
    expected=$2
    echo "Checking $url"
    actual=$(curl -s "$url")
    if ! echo "$actual" | grep -q "$expected"
    then
        echo "Expected output not found in server response"
        echo "Expected: $expected"
        echo "Actual: $actual"
        test_failed=yes
    fi
}

# Run the tests

case "$@" in
    --fast)
        echo "Skipping wrk test"
        ;;
    *)
        duration=2
        printf "Running wrk test for %s seconds ..." $duration
        wrk -d$duration 'http://localhost:3000' > wrk.output
        printf " done\n"
        ;;
esac

curlcheck 'http://localhost:3000' 'Hello, world!'
curlcheck 'http://localhost:3000/goober' 'Hello, goober!'
curlcheck 'http://localhost:3000/foo?style=quiet' 'hi foo'
curlcheck 'http://localhost:3000/bar?style=loud' 'HELLO, BAR!!!'
curlcheck 'http://localhost:3000/baz?style=nope' 'Bad style'

# Check the output
if grep -q "ERROR" $testout
then
    echo "Error log in server output"
    test_failed=yes
fi

kill $pid

if [ $test_failed = yes ]
then
    echo "Test failed"
    echo "Server output:"
    cat $testout
    exit 1
fi

echo "Test passed"
