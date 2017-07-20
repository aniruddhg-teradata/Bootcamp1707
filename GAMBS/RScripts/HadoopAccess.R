install.packages("/mnt/r-stuff/rHadoopClient_0.2.tar.gz", repos=NULL, type="source")
library(rHadoopClient)
rHadoopClient::read.hdfs("hdfs://54.208.153.19:8020/landingzone/pov1999")
