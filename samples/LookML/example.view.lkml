- view: view_name
  sql_table_name: table_name
  suggestions: true | false

  derived_table:
    sql: SQL query
    persist_for: N (seconds | minutes | hours)
    sql_trigger_value: SQL query
    distribution: column_name
    distribution_style: ALL | EVEN
    sortkeys: [column_name, column_name, …]
    indexes: [column_name, column_name, …]

  sets:
    set_name:
      - field_or_set
      - field_or_set
      - …
    # Possibly more set declarations

  fields:
  - (dimension | dimension_group | measure | filter): field_name
    label: 'desired label name'
    view_label: 'desired label name'
    group_label: 'desired label name'
    description: 'description string'
    hidden: true | false
    alias: [old_field_name, old_field_name, …]
    value_format: 'excel-style formatting string'
    value_format_name: format_name
    html: HTML expression using Liquid template elements
    sql: SQL expression to generate the field value
    required_fields: [field_name, field_name, …]
    drill_fields: [field_or_set, field_or_set, …]
    can_filter: true | false
    fanout_on: repeated_record_name

    # DIMENSION SPECIFIC PARAMETERS

    type: dimension_field_type
    primary_key: true | false
    sql_case:
      value: SQL condition
      value: SQL condition
      # Possibly more sql_case statements
    alpha_sort: true | false
    tiers: [N, N, …]
    style: classic | interval | integer | relational
    sql_latitude: SQL expression to generate a latitude
    sql_longitude: SQL expression to generate a longitude
    suggestable: true | false
    suggest_persist_for: N (seconds | minutes | hours)
    suggest_dimension: dimension_name
    suggest_explore: explore_name
    suggestions: ['suggestion string', 'suggestion string', …]
    bypass_suggest_restrictions: true | false
    full_suggestions: true | false
    skip_drill_filter: true | false
    case_sensitive: true | false
    order_by_field: dimension_name
    map_layer: name_of_map_layer
    links:
      - label: 'desired label name'
        url: desired_url
        icon_url: url_of_an_ico_file
      # Possibly more links

    # DIMENSION GROUP SPECIFIC PARAMETERS

    timeframes: [timeframe, timeframe, …]
    convert_tz: true | false
    datatype: epoch | timestamp | datetime | date | yyyymmdd

    # MEASURE SPECIFIC PARAMETERS

    type: measure_field_type
    direction: row | column
    approximate: true | false
    approximate_threshold: N
    sql_distinct_key: SQL expression to define repeated entities
    list_field: dimension_name
    filters:
      dimension_name: 'looker filter expression'
      # Possibly more filters statements

    # FILTER SPECIFIC PARAMETERS

    default_value: 'desired default value'

  # Possibly more dimension or measure declarations
