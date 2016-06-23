#!/bin/sh
#
# limitations under the License.
#

GATLING_CONF="${GATLING_CONF:=$GATLING_HOME/conf}"

JAVA_OPTS="-server -XX:+UseThreadPriorities -XX:ThreadPriorityPolicy=42 -Xms512M -Xmx512M -Xmn100M -XX:+HeapDumpOnOutOfMemoryError -XX:+AggressiveOpts -XX:+OptimizeStringConcat -XX:+UseFastAccessorMethods -XX:+UseParNewGC -XX:+UseConcMarkSweepGC -XX:+CMSParallelRemarkEnabled -Djava.net.preferIPv4Stack=true -Djava.net.preferIPv6Addresses=false ${JAVA_OPTS}"

# Build compilation classpath
COMPILATION_CLASSPATH=`find $GATLING_HOME/lib -maxdepth 1 -name "*.jar" -type f -exec printf :{} ';'`

java $JAVA_OPTS -cp "$GATLING_CLASSPATH" io.gatling.app.Gatling $USER_ARGS
