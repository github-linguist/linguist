metadata {
    version = "1.0.0"
    author = "Symbiont Community"
    description = "Schema validation and data quality assessment engine"
    tags = ["validation", "quality", "schema", "compliance"]
}

agent data_validator(data: DataSet, schema: ValidationSchema) -> ValidationReport {
    capabilities = ["data_validation", "schema_checking", "quality_assessment"]

    policy data_quality {
        allow: ["read_data", "validate_schema", "quality_check"]
            if data.source.trusted == true && data.records.length <= 100000
        deny: ["write_data", "execute_code", "network_access"]

        require: {
            schema_validation: true,
            input_sanitization: true,
            max_data_size: "100MB"
        }

        audit: {
            log_level: "info",
            include_input: false,  // Protect PII in data
            include_statistics: true,
            include_validation_errors: true,
            compliance_tags: ["data-quality", "GDPR"]
        }
    }

    with
        memory = "ephemeral",
        privacy = "high",
        security = "high",
        sandbox = "Tier1",
        timeout = 30000,
        max_memory_mb = 1024,
        max_cpu_cores = 1.0
    {
        try {
            // Validate inputs
            if data.records.length == 0 {
                return ValidationReport {
                    valid_records: 0,
                    invalid_records: 0,
                    errors: [],
                    warnings: ["No records to validate"],
                    quality_score: 0.0
                };
            }

            report = ValidationReport {
                valid_records: 0,
                invalid_records: 0,
                errors: [],
                warnings: [],
                quality_score: 0.0
            };

            for record in data.records {
                validation_result = validate_against_schema(record, schema);

                if validation_result.valid {
                    report.valid_records += 1;
                } else {
                    report.invalid_records += 1;
                    report.errors.append(validation_result.errors);
                }
            }

            let total = report.valid_records + report.invalid_records;
            report.quality_score = if total > 0 {
                report.valid_records / total
            } else {
                0.0
            };

            return report;

        } catch (error) {
            log("ERROR", "Validation failed: " + error.message);
            return ValidationReport {
                valid_records: 0,
                invalid_records: 0,
                errors: [error.message],
                warnings: [],
                quality_score: 0.0
            };
        }
    }
}
