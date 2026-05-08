metadata {
    version = "1.0.0"
    author = "Symbiont Community"
    description = "Multi-source API aggregator with parallel fetching and normalization"
    tags = ["api", "aggregation", "data-integration", "http"]
}

agent api_aggregator(sources: Array<APISource>, query: String) -> AggregatedData {
    capabilities = ["api_access", "data_aggregation", "response_merging"]

    policy api_access {
        allow: ["https_request"] if request.url in sources.map(s => s.endpoint)
        deny: ["http_request", "execute_code", "file_access"]

        require: {
            tls_verification: true,
            api_key_rotation: "30_days",
            rate_limiting: "1000/hour",
            timeout_per_request: "3000ms"
        }

        audit: {
            log_level: "info",
            include_request_headers: false,  // May contain API keys
            include_response_status: true,
            include_latency: true,
            alert_on_errors: true
        }
    }

    with
        memory = "ephemeral",
        security = "high",
        sandbox = "Tier1",
        timeout = 30000,
        max_memory_mb = 512,
        max_cpu_cores = 1.0
    {
        try {
            results = [];

            for source in sources {
                try {
                    response = http.get(source.endpoint, {
                        query: query,
                        headers: {
                            "Authorization": "Bearer " + vault://api/keys[source.auth_key],
                            "Content-Type": "application/json"
                        },
                        timeout: 3000,
                        verify_tls: true
                    });

                    standardized_data = normalize_response(response.body, source.schema);
                    results.append({
                        "source": source.name,
                        "data": standardized_data,
                        "timestamp": now()
                    });

                } catch (APIError e) {
                    log("WARNING", "API call failed for source: " + source.name);
                    results.append({
                        "source": source.name,
                        "error": e.message,
                        "timestamp": now()
                    });
                }
            }

            return AggregatedData {
                query: query,
                sources_queried: sources.length,
                successful_responses: results.filter(r => r.data != null).length,
                aggregated_results: merge_results(results)
            };

        } catch (error) {
            log("ERROR", "API aggregation failed: " + error.message);
            return error("Aggregation failed: " + error.message);
        }
    }
}
