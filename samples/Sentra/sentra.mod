module github.com/sentra-security/scanner

require (
    github.com/sentra-security/core v1.0.0
    github.com/sentra-security/network v1.2.0
    github.com/sentra-security/siem v0.9.0
)

replace github.com/sentra-security/core => ./vendor/core

security {
    scan_level = "deep"
    enable_heuristics = true
}