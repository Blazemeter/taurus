import os
import sys
import platform

print("Fake gatling output")
print("dir:")
print(os.path.abspath(os.curdir))

if len(sys.argv) == 2 and sys.argv[1] == '--help':
    exit(0)

java_opts = os.environ.get("JAVA_OPTS", "").strip() + " "

res_dir_opt = "gatling.core.directory.resources"
start_res_dir = java_opts.find(res_dir_opt) + len(res_dir_opt) + 1
res_dir_str = java_opts[start_res_dir:]
res_dir = res_dir_str[:res_dir_str.find(" -D")]
if platform.system() != "Windows":
    res_dir = res_dir[1:-1]

scala_files = [fname for fname in os.listdir(res_dir) if fname.endswith('scala')]
if len(scala_files) > 1:
    print("Choose a simulation number:")
    input()

print("started...")

default_gatling_home = "False"
gatling_home = os.environ.get("GATLING_HOME", default_gatling_home)
os.environ["GATLING_HOME"] = gatling_home

java_opts = "-server -XX:+UseThreadPriorities -XX:ThreadPriorityPolicy=42 -Xms512M -Xmx512M -Xmn100M "
java_opts += "-XX:+HeapDumpOnOutOfMemoryError -XX:+AggressiveOpts -XX:+OptimizeStringConcat "
java_opts += "-XX:+UseFastAccessorMethods -XX:+UseParNewGC -XX:+UseConcMarkSweepGC -XX:+CMSParallelRemarkEnabled "
java_opts += "-Djava.net.preferIPv4Stack='true' -Djava.net.preferIPv6Addresses='false' "
java_opts += os.environ.get("JAVA_OPTS", "")
os.environ["JAVA_OPTS"] = java_opts

jars = []
jar_dir = os.path.join(gatling_home, "lib")

if os.path.isdir(jar_dir):
    for fname in os.listdir(jar_dir):
        if os.path.isfile(fname) and fname.endswith(".jar"):
            jars.append(fname)

compilation_classpath = ';'.join(jars)
os.environ["COMPILATION_CLASSPATH"] = ':'.join((compilation_classpath, os.environ.get("COMPILATION_CLASSPATH", "")))

print(os.environ.get("GATLING_HOME", ""))
print(os.environ.get("COMPILATION_CLASSPATH", ""))
print(os.environ.get("NO_PAUSE", ""))
print(os.environ.get("JAVA_OPTS", ""))
