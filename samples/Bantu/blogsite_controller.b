// ============================================================================
// controller.b — Request handlers for the Bantu Blogsite sample
//
// Each handler receives ($req, $res) and writes the response. Re-uses
// the db.b module's symbols (initDb, listPosts, getPost, createPost).
// ============================================================================

postController = {
    "index": def($req, $res) {
        $posts = listPosts();
        $res.json({ "ok": true, "count": $posts.size(), "posts": $posts });
    },
    "show": def($req, $res) {
        $id = $req.params["id"];
        $post = getPost($id);
        if ($post == null) {
            $res.status(404);
            $res.json({ "ok": false, "error": "post not found" });
            return;
        }
        $res.json({ "ok": true, "post": $post });
    },
    "create": def($req, $res) {
        $title = $req.body["title"];
        $body  = $req.body["body"];
        if ($title == "" || $title == null) {
            $res.status(400);
            $res.json({ "ok": false, "error": "title is required" });
            return;
        }
        createPost($title, $body);
        $res.status(201);
        $res.json({ "ok": true, "message": "post created" });
    }
};

print("[controller] registered 3 handlers: index, show, create");
