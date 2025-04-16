- label:          'desired label name'
- connection:     connection_name
- include:        filename_or_pattern
  # Possibly more include declarations
- persist_for:    N (seconds | minutes | hours)
- case_sensitive: true | false
- week_start_day: monday | tuesday | wednesday | thursday | friday | saturday | sunday
- value_formats:
  - name: desired_format_name
    value_format: 'excel-style formatting string'
  # Possibly more value formats

- explore: view_name
  label:  'desired label name'
  description: 'description string'
  symmetric_aggregates: true | false
  hidden: true | false
  fields: [field_or_set, field_or_set, …]

  sql_always_where: SQL WHERE condition
  always_filter:
    field_name: 'looker filter expression'
  conditionally_filter:
    field_name: 'looker filter expression'
    unless: [field_or_set, field_or_set, …]
  access_filter_fields: [fully_scoped_field, fully_scoped_field, …]

  always_join: [view_name, view_name, …]
  joins:
    - join: view_name
      type: left_outer | full_outer | inner | cross
      relationship: one_to_one | many_to_one | one_to_many | many_to_many
      from: view_name
      sql_table_name: table_name
      view_label: 'desired label name'
      fields: [field_or_set, field_or_set, …]
      required_joins: [view_name, view_name, …]
      foreign_key: dimension_name
      sql_on: SQL ON clause
    # Possibly more join declarations

  persist_for: N (seconds | minutes | hours)
  from: view_name
  view: view_name
  case_sensitive: true | false
  sql_table_name: table_name
  cancel_grouping_fields: [fully_scoped_field, fully_scoped_field, …]

# Possibly more explore declarations
