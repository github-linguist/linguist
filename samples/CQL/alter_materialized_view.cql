ALTER MATERIALIZED VIEW cycling.cyclist_by_age
WITH comment = 'A most excellent and useful view'
AND bloom_filter_fp_chance = 0.02;

ALTER MATERIALIZED VIEW cycling.cyclist_by_age
WITH compression = {
    'sstable_compression' : 'DeflateCompressor',
    'chunk_length_kb' : 64
}
AND compaction = {
    'class' : 'SizeTieredCompactionStrategy',
    'max_threshold' : 64
};

ALTER MATERIALIZED VIEW cycling.cyclist_by_age
WITH caching = {
    'keys' : 'NONE',
    'rows_per_partition' : '15'
};
