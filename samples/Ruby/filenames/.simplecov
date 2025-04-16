SimpleCov.add_group 'db', '/lib/mygem/db/'
SimpleCov.add_group 'utils', '/lib/mygem/utils/'
SimpleCov.add_group 'spec', '/spec/'

SimpleCov.coverage_dir ENV.fetch('COVERAGE_DIR', 'coverage')
SimpleCov.command_name $PROGRAM_NAME
SimpleCov.merge_timeout 3600
