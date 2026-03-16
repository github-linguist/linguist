CREATE KEYSPACE IF NOT EXISTS cycling
WITH REPLICATION = {
    'class' : 'SimpleStrategy',
    'replication_factor' : 1
};

CREATE KEYSPACE food_cql
WITH REPLICATION = {
    'class': 'SimpleStrategy',
    'replication_factor': 1
}
AND graph_engine = 'Core';

CREATE KEYSPACE cycling
WITH REPLICATION = {
    'class' : 'NetworkTopologyStrategy',
    'dc1' : 1
};

CREATE KEYSPACE IF NOT EXISTS cycling
WITH REPLICATION = {
    'class' : 'NetworkTopologyStrategy',
    'datacenter1': '3',
    'datacenter2': '3'
};

CREATE KEYSPACE cycling
WITH REPLICATION = {
    'class' : 'NetworkTopologyStrategy',
    'datacenter1' : 3
}
AND DURABLE_WRITES = false;
