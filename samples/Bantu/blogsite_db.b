// ============================================================================
// db.b — Database module for the Bantu Blogsite sample
// Exposes:
//   initDb()         — create posts table if missing
//   $db              — handle to the sua.sqlite driver namespace
//   listPosts()      — return all posts
//   getPost($id)     — return one post or null
//   createPost($title, $body) — insert a post, return the new row id
// ============================================================================

$db = sua.sqlite;

def initDb() {
    $db.connect("blogsite.db");
    $db.exec("CREATE TABLE IF NOT EXISTS posts ("
        + "id INTEGER PRIMARY KEY AUTOINCREMENT, "
        + "title TEXT NOT NULL, "
        + "body TEXT NOT NULL, "
        + "author TEXT DEFAULT 'anonymous', "
        + "created_at TEXT DEFAULT CURRENT_TIMESTAMP)");
    print("[db] initialized — posts table ready");
}

def listPosts() {
    return $db.query("SELECT id, title, author, created_at FROM posts ORDER BY id DESC");
}

def getPost($id) {
    $rows = $db.query("SELECT * FROM posts WHERE id = " + $id);
    if ($rows.size() == 0) { return null; }
    return $rows[0];
}

def createPost($title, $body) {
    $sql = "INSERT INTO posts (title, body) VALUES ('"
        + $title + "', '" + $body + "')";
    $db.exec($sql);
    return "post created";
}
