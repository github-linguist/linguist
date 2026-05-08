metadata {
    version = "1.0.0"
    author = "Symbiont Community"
    description = "Multi-agent code review pipeline: untrusted developer in strict sandbox passes code to AgentPin-authenticated reviewer"
    tags = ["multi-agent", "code-review", "agentpin", "sandbox", "trust-boundary"]
}

# Two-agent pipeline demonstrating trust boundaries:
#
# 1. untrusted_developer — confined to Tier2 (gVisor), can only read
#    and analyze code. Cannot execute, deploy, or access network.
#
# 2. trusted_reviewer — authenticated via AgentPin ES256 credentials.
#    Can sign approved code and authorize execution. Only accepts
#    input from verified agents.
#
# The CommunicationPolicyGate enforces that the untrusted developer
# can only send to the reviewer, never bypass it.

agent untrusted_developer(code: String, language: String) -> ReviewRequest {
    capabilities = ["read", "analyze", "lint", "format"]

    policy strict_sandbox {
        // Read and analyze only
        allow: read(file) if file.in_project == true
        allow: analyze(code) if true
        allow: lint(code) if true
        allow: format(code) if true

        // Cannot execute any code
        deny: execute(any)
        deny: invoke_tool(tool) if tool.name == "Bash"
        deny: invoke_tool(tool) if tool.name == "Write"

        // Cannot access network
        deny: network_access(any)
        deny: web_request(any)

        // Can only communicate with trusted_reviewer
        allow: delegate(agent) if agent.name == "trusted_reviewer"
        deny: delegate(agent) if agent.name != "trusted_reviewer"
        deny: send_to(agent) if agent.name != "trusted_reviewer"

        audit: {
            log_level: "info",
            include_all_tool_calls: true,
            include_code_diffs: false,    // don't log full code in audit
            include_policy_decisions: true
        }
    }

    with
        sandbox = "Tier2",          // gVisor — untrusted code isolation
        memory = "ephemeral",       // no state persistence
        timeout = 120000,           // 2 minutes
        max_memory_mb = 512,
        max_cpu_cores = 1.0,
        network_policy = "deny_all"
    {
        // Step 1: Static analysis (no execution)
        let analysis = analyze(code, language);

        // Step 2: Lint for common issues
        let lint_results = lint(code, {
            language: language,
            rules: ["security", "best-practices", "complexity"]
        });

        // Step 3: Check for dangerous patterns
        let security_findings = [];
        let patterns = [
            { name: "shell_injection", regex: "exec\\(|system\\(|popen\\(" },
            { name: "sql_injection", regex: "format!.*SELECT|query.*\\+" },
            { name: "path_traversal", regex: "\\.\\./|\\.\\.\\\\"},
            { name: "hardcoded_secrets", regex: "password\\s*=|api_key\\s*=|secret\\s*=" }
        ];

        for pattern in patterns {
            if code.contains_pattern(pattern.regex) {
                security_findings.push({
                    type: pattern.name,
                    severity: "high",
                    recommendation: "Review and remediate before approval"
                });
            }
        }

        // Step 4: Build review request for trusted reviewer
        let review_request = ReviewRequest {
            code: code,
            language: language,
            complexity_score: analysis.complexity,
            lint_issues: lint_results.issues,
            security_findings: security_findings,
            recommendation: if security_findings.len() == 0 {
                "APPROVE"
            } else {
                "REVIEW_REQUIRED"
            },
            developer_agent: "untrusted_developer",
            timestamp: now()
        };

        // Step 5: Send to trusted reviewer via CommunicationBus
        let reviewer_response = delegate("trusted_reviewer", {
            action: "review",
            request: review_request
        });

        return reviewer_response;
    }
}

agent trusted_reviewer(request: ReviewRequest) -> ReviewDecision {
    capabilities = ["read", "analyze", "sign", "approve", "execute_tests"]

    policy authenticated_reviewer {
        // Require AgentPin authentication
        require: {
            agentpin_verified: true,
            agentpin_algorithm: "ES256",
            identity_domain: "thirdkey.ai"
        }

        // Only accept input from known agents
        allow: receive(message) if message.sender.agentpin_verified == true
        deny: receive(message) if message.sender.agentpin_verified == false

        // Can run tests in isolated environment
        allow: execute_tests(code) if code.reviewed == true
        deny: execute(code) if code.reviewed == false

        // Can sign approved code
        allow: sign(artifact) if artifact.review_status == "approved"

        // Cannot modify source directly
        deny: write(file) if file.is_source_code == true

        audit: {
            log_level: "info",
            include_review_decisions: true,
            include_approval_chain: true,
            include_signatures: true
        }
    }

    with
        sandbox = "Tier1",          // Docker — trusted but isolated
        memory = "persistent",      // remember review history
        timeout = 60000,
        max_memory_mb = 1024
    {
        // Step 1: Verify the request came from an authenticated agent
        if !request.developer_agent.agentpin_verified {
            return ReviewDecision {
                status: "REJECTED",
                reason: "Request from unverified agent",
                signed: false
            };
        }

        // Step 2: Review security findings
        if request.security_findings.len() > 0 {
            let critical = request.security_findings.filter(f => f.severity == "critical");
            if critical.len() > 0 {
                return ReviewDecision {
                    status: "REJECTED",
                    reason: "Critical security findings: " + critical.map(f => f.type).join(", "),
                    findings: request.security_findings,
                    signed: false
                };
            }
        }

        // Step 3: Run tests if analysis looks clean
        let test_results = {};
        if request.recommendation == "APPROVE" {
            try {
                test_results = execute_tests(request.code, {
                    language: request.language,
                    timeout: 30000,
                    sandbox: "Tier2"  // tests run in stricter sandbox
                });
            } catch (test_error) {
                return ReviewDecision {
                    status: "REJECTED",
                    reason: "Tests failed: " + test_error.message,
                    signed: false
                };
            }
        }

        // Step 4: Approve and sign
        let decision = ReviewDecision {
            status: "APPROVED",
            reviewer: "trusted_reviewer",
            code_hash: hash_sha256(request.code),
            lint_issues: request.lint_issues.len(),
            security_findings: request.security_findings.len(),
            test_results: test_results,
            timestamp: now()
        };

        // Sign the approval with AgentPin credentials
        decision.signature = sign_with_agentpin(decision);
        decision.signed = true;

        log("INFO", format(
            "Code review APPROVED: {} ({} lint issues, {} security findings, hash: {})",
            request.language,
            decision.lint_issues,
            decision.security_findings,
            decision.code_hash
        ));

        return decision;
    }
}
