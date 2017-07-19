var sheriffCrimes = spark.read.csv("hdfs:///evil/data/sheriffCrimes.csv")
var sheriffCrimes2016 = sheriffCrimes.select("_c0", "_c1", "_c4").filter(sheriffCrimes("_c2") === "2016").filter(sheriffCrimes("_c0") !== "null")

sheriffCrimes2016.createOrReplaceTempView("sheriffCrimesTable")
var crimesView = spark.sql("select _c0 as zip, DATE_FORMAT(datetimestamp, 'yyyy-MM-dd') as date, DATE_FORMAT(datetimestamp, 'HH:mm:ss') as time, _c4 as crime from (select *, CAST(UNIX_TIMESTAMP(_c1, 'MM/dd/yyyy HH:mm:ss a') AS TIMESTAMP) as datetimestamp from sheriffCrimesTable)")

crimesView.write.csv("/evil/data/crimesView")
