// ============================================================================
// db.b — PostgreSQL layer for the PG Dashboard sample
// ============================================================================

$pg = sua.postgres;

def initSchema() {
    $pg.exec("CREATE TABLE IF NOT EXISTS events ("
        + "id SERIAL PRIMARY KEY, "
        + "kind TEXT NOT NULL, "
        + "value INTEGER NOT NULL, "
        + "created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP)");
    // Seed some sample data
    $rows = $pg.query("SELECT COUNT(*) AS n FROM events");
    if ($rows[0]["n"] == "0") {
        $pg.exec("INSERT INTO events (kind, value) VALUES "
            + "('page_view', 1), ('click', 1), ('signup', 1), "
            + "('page_view', 1), ('click', 1), ('purchase', 1)");
        print("[db] seeded 6 sample events");
    } else {
        print("[db] events table has " + $rows[0]["n"] + " rows");
    }
}

def metrics() {
    $byKind = $pg.query(
        "SELECT kind, SUM(value) AS total, COUNT(*) AS count "
        + "FROM events GROUP BY kind ORDER BY total DESC");
    $totalRow = $pg.query("SELECT COUNT(*) AS n, COALESCE(SUM(value),0) AS sum FROM events");
    return {
        "totalEvents": $totalRow[0]["n"],
        "totalValue": $totalRow[0]["sum"],
        "byKind": $byKind
    };
}

def trackEvent($kind, $value) {
    $pg.exec("INSERT INTO events (kind, value) VALUES ('" + $kind + "', " + $value + ")");
    return "tracked";
}
