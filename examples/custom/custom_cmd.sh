#! /bin/bash -e

# This is dummy command that will accept some parameters and will generate results file
# Remember that Bash is terrible for complex scripts, use something like Python or JS for real custom load generators


# Parameters:
RESULTS=$1 # where to write results
CONCURR=$2 # concurrency value, not really used, demonstrates passing it into script
ITERATS=$3 # how many iteration to perform
PAYLOAD=$4 # also not really used, but demonstrates passing "scenario" part to custom tool

echo RESULTS=$RESULTS
echo CONCURR=$CONCURR
echo ITERATS=$ITERATS
echo PAYLOAD=$PAYLOAD

# first, we write a CSV header, using JTL field set to re-use JTLReader on Taurus side
echo Writing JTL header...
echo "timeStamp,elapsed,label,responseCode,responseMessage,threadName,success,bytes,grpThreads,allThreads,Latency,Hostname,Connect" > $RESULTS

# then we just do as many iters, writing samples for each iter
echo Starting iterations...

for I in $(seq 1 $ITERATS); do
    for K in $(seq 1 $[ ( $RANDOM % $CONCURR ) + 1 ]); do
        CON=$[ ( $RANDOM % 100 ) + 1 ]
        LAT=$[ ( $RANDOM % 200 ) + $CON ]
        ELP=$[ ( $RANDOM % 1000 ) + $LAT ]
        RC=$[ ( $RANDOM % 4 ) + 2 ]00
        BYT=$[ ( $RANDOM % 1000 ) + 1 ]
        if (( RC > 200 )); then SUC=false; else SUC=true; fi
        echo `date +%s%j`,$ELP,label-$K,$RC,responseMessage,threadName,$SUC,$BYT,$CONCURR,$CONCURR,$LAT,`hostname`,$CON >> $RESULTS
        sleep 0.$[ ( $RANDOM % 10 ) + 1 ]
    done
    (>&2 echo "Demo of writing to stderr #$I")
done

echo Done iterations.
