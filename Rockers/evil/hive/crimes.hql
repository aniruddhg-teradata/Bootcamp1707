CREATE EXTERNAL TABLE IF NOT EXISTS crimes (
DRNumber INT,
DateReported STRING,
DateOccurred STRING,
TimeOccurred INT,
AreaID INT,
AreaName STRING,
ReportingDistrict INT,
CrimeCode INT,
CrimeCodeDescription STRING,
MOCodes STRING,
VictimAge INT,
VictimSex STRING,
VictimDescent STRING,
PremiseCode INT,
PremiseDescription STRING,
WeaponUsedCode INT,
WeaponDescription STRING,
StatusCode STRING,
StatusDescription STRING,
CrimeCode1 INT,
CrimeCode2 INT,
CrimeCode3 INT,
CrimeCode4 INT,
Address STRING,
CrossStreet STRING,
Location STRING
)
ROW FORMAT SERDE 'com.bizo.hive.serde.csv.CSVSerde'
WITH SERDEPROPERTIES (
   "separatorChar" = ",",
   "quoteChar"     = "\"",
   "escapeChar"    = "\\"
  )   
STORED AS TEXTFILE
LOCATION '/data/evil';