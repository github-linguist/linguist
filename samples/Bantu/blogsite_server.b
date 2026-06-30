// ============================================================================
// server.b — Entry point for the Bantu Blogsite sample (v1.2.2)
//
// Demonstrates:
//   - Modular structure with `include` keyword
//   - Sua HTTP server (Express-like API)
//   - Sua SQLite persistence
//   - Reusable route + controller modules
// ============================================================================

print("=== Bantu Blogsite v1.2.2 ===");

// 1. Load shared utilities and DB layer
include "./db.b";              // brings $db, initDb() into scope
include "./controller.b";       // brings postController into scope
include "./routes.b" as routes; // namespaced include — exposes routes.*

// 2. Initialize the database (creates posts table if needed)
initDb();

// 3. Register routes — routes.registerAll(sua)
routes.registerAll(sua);

// 4. Start the HTTP server
sua.server.static("./public");
sua.server.listen(3000);
