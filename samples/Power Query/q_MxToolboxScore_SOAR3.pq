/*
==============================================================================
 Query Name : q_MxToolboxScore_SOAR3
 SOAR Stage : SOAR 3 – Deterministic Risk Scoring
 Load To    : Connection Only
 Expansion  : NONE (Scalar only)
 Purpose    : Compute a deterministic risk score and tier classification
              from SOAR‑3 scalar MXToolbox indicators.
 Guardrails :
   - MUST be explainable and auditable
   - MUST NOT infer maliciousness beyond documented tiers
==============================================================================
*/
let
    Source = q_MxToolboxOutput_SOAR3,

    Scored =
        Table.AddColumn(
            Source,
            "RiskScore",
            each
                // Failures
                (if [FailedCount] = 0 then 0
                 else if [FailedCount] <= 2 then 30
                 else 60)

                // Warnings
              + (if [WarningsCount] = 0 then 0
                 else if [WarningsCount] <= 3 then 10
                 else 20)

                // Timeouts
              + (if [TimeoutsCount] > 0 then 10 else 0)

                // DMARC
              + (if [HasDmarcRecord] = true then 0 else 20),
            Int64.Type
        ),

    Tiered =
        Table.AddColumn(
            Scored,
            "ReputationTier",
            each
                if [RiskScore] < 10 then "T1_Clean"
                else if [RiskScore] < 30 then "T2_Degraded"
                else if [RiskScore] < 60 then "T3_HighRisk"
                else "T4_Malicious",
            type text
        )
in
    Tiered
