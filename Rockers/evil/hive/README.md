Add the csv-serde to hive aux jars at runtime
$> hive --hiveconf hive.aux.jars.path=/home/gpadmin/3845/csv-serde-0.9.1.jar
Place csv-serde-0.9.1.jar in a commmon direcotry on the hive client node and set hive.aux.jars.path in /etc/gphd/hive/conf/hive-site.xml
Add the jar file during runtime
hive> add jar /tmp/csv-serde-0.9.1.jar;

add jar /home/hadoop/evil/lib/csv-serde-1.1.2-0.11.0-all.jar;

