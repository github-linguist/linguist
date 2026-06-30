// ============================================================================
// routes.b — HTTP route registration for the POS system
// ----------------------------------------------------------------------------
// Exposes:
//   registerAll($sua)
//
// Note: Bantu v1.2.2 does not support anonymous lambdas (`def($x) { ... }`)
// as expression-position values. We declare each handler as a NAMED function
// at module scope, then pass the function name to sua.server.get/post/put/
// delete. When the route matches, the Sua server invokes the named function
// with ($req, $res).
// ============================================================================

// ── Handler functions (one per route) ────────────────────────────────────

def healthHandler($req, $res) {
    $res.json({
        "ok": true,
        "service": "bantu-pos",
        "version": "1.0.0",
        "database": "postgresql"
    });
}

def listProductsHandler($req, $res) {
    $q = $req.query["q"];
    if ($q == null) { $q = ""; }
    $rows = listProducts($q);
    $res.json({ "ok": true, "count": len($rows), "products": $rows });
}

def getProductHandler($req, $res) {
    $p = getProduct($req.params["id"]);
    if ($p == null) {
        $res.status(404);
        $res.json({ "ok": false, "error": "Product not found" });
        return null;
    }
    $res.json({ "ok": true, "product": $p });
}

def lookupProductHandler($req, $res) {
    $p = findByCode($req.params["code"]);
    if ($p == null) {
        $res.status(404);
        $res.json({ "ok": false, "error": "No product matches that barcode or SKU" });
        return null;
    }
    $res.json({ "ok": true, "product": $p });
}

def createProductHandler($req, $res) {
    $p = $req.body;
    if ($p["name"] == null || $p["name"] == "" || $p["sku"] == null || $p["sku"] == "") {
        $res.status(400);
        $res.json({ "ok": false, "error": "name and sku are required" });
        return null;
    }
    $created = createProduct($p);
    if ($created == null) {
        $res.status(500);
        $res.json({ "ok": false, "error": "Could not create product (duplicate SKU?)" });
        return null;
    }
    $res.status(201);
    $res.json({ "ok": true, "product": $created });
}

def updateProductHandler($req, $res) {
    $updated = updateProduct($req.params["id"], $req.body);
    if ($updated == null) {
        $res.status(404);
        $res.json({ "ok": false, "error": "Product not found" });
        return null;
    }
    $res.json({ "ok": true, "product": $updated });
}

def deleteProductHandler($req, $res) {
    deleteProduct($req.params["id"]);
    $res.json({ "ok": true });
}

def listSalesHandler($req, $res) {
    $lim = $req.query["limit"];
    if ($lim == null) { $lim = 100; }
    $rows = listSales($lim);
    $res.json({ "ok": true, "count": len($rows), "sales": $rows });
}

def getSaleHandler($req, $res) {
    $s = getSale($req.params["id"]);
    if ($s == null) {
        $res.status(404);
        $res.json({ "ok": false, "error": "Sale not found" });
        return null;
    }
    $res.json({ "ok": true, "sale": $s });
}

def checkoutHandler($req, $res) {
    $payload = $req.body;
    if ($payload == null || $payload["items"] == null || len($payload["items"]) == 0) {
        $res.status(400);
        $res.json({ "ok": false, "error": "Cart is empty" });
        return null;
    }
    $sale = createSale($payload);
    if ($sale == null) {
        $res.status(500);
        $res.json({ "ok": false, "error": "Could not complete sale (check item IDs)" });
        return null;
    }
    $res.status(201);
    $res.json({ "ok": true, "sale": $sale });
}

def dashboardHandler($req, $res) {
    $m = dashboard();
    $res.json({ "ok": true, "metrics": $m });
}

// ── Route registration ───────────────────────────────────────────────────

def registerAll($sua) {
    // Health
    $sua.server.get("/api/health",                  healthHandler);

    // Products
    $sua.server.get("/api/products",                listProductsHandler);
    $sua.server.get("/api/products/:id",            getProductHandler);
    $sua.server.get("/api/products/lookup/:code",   lookupProductHandler);
    $sua.server.post("/api/products",               createProductHandler);
    $sua.server.put("/api/products/:id",            updateProductHandler);
    $sua.server.delete("/api/products/:id",         deleteProductHandler);

    // Sales
    $sua.server.get("/api/sales",                   listSalesHandler);
    $sua.server.get("/api/sales/:id",               getSaleHandler);
    $sua.server.post("/api/checkout",               checkoutHandler);

    // Dashboard
    $sua.server.get("/api/dashboard",               dashboardHandler);

    print("[routes] registered 11 routes under /api/*");
}
