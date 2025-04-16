SET hive.tez.container.size = 2048;
set hive.optimize.sort.dynamic.partition=true;

SET hivevar:DB=test;

create database if not exists ${DB};

-- External CSV table
CREATE EXTERNAL TABLE ${DB}.users (
  id STRING,
  name STRING,
  age INT
)
COMMENT 'External table mapping CSV files'
ROW FORMAT DELIMITED
FIELDS TERMINATED BY ','
NULL DEFINED AS ''
STORED AS TEXTFILE
LOCATION 'hdfs:///some/hdfs/path'
TBLPROPERTIES ('skip.header.line.count' = '1');
MSCK REPAIR TABLE ${DB}.methodos_csv SYNC PARTITIONS;

-- ORC table
create table ${DB}.purchases
as
  select product, price, user_id
from ${DB}.purchases_extended
stored as orc
tblproperties ('orc.compress'='ZLIB');

-- Join query
SELECT name, sum(price) as total_spent, count(product) as nb_products
FROM ${DB}.users
LEFT JOIN ${DB}.purchases
GROUP BY name;
