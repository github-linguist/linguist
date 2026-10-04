# Home controller - handles routes at /

class HomeController extends Controller
    # GET /up
    def up
        render_text("UP")
    end

    # GET /
    def index
        render("home/index", {
            "title": "Welcome",
            "message": "The Modern MVC Framework for Soli",
            "meta": {
                "title": "Soli — Write it like Ruby. Serve it like Rust.",
                "description": "Soli is a programming language with a web framework built in: models, auth, jobs, mail, PDF, live views and tests in one binary, at 1.2M+ requests/s.",
                "path": "/"
            }
        }, {"layout": "layouts/landing"})
    end

    # GET /ai
    def ai
        evals = {
            "generated_at": null,
            "harness": "soli-evals/0.2",
            "runs_per_model": 3,
            "models": [],
            "tasks": [],
            "note": "No paid run committed yet."
        }
        path = "data/ai_evals.json"
        path = "www/data/ai_evals.json" unless File.exists(path)
        if File.exists(path)
            evals = json_parse(File.read(path)) rescue evals
        end
        render("home/ai", {
            "title": "Soli is built for AI",
            "evals": evals,
            "meta": {
                "title": "Soli is built for AI — agents, RAG and a code graph",
                "description": "One way to do each thing, a short language and a full stack: code agents write in few tokens and humans can review, with the LLM and retrieval layer included.",
                "path": "/ai"
            }
        }, {"layout": "layouts/landing"})
    end

    # GET /health
    def health
        render_json({
            "status": "ok"
        })
    end

    # GET /docs - redirect to documentation
    def docs_redirect
        {
            "status": 302,
            "headers": {"Location": "/docs.html"},
            "body": ""
        }
    end

    # GET /files/*filepath - Splat route demo
    def files_demo
        render_json({
            "route": "files_demo",
            "params": req["params"]
        })
    end

    # GET /api/*version/users/*id - Multi-splat route demo
    def api_demo
        render_json({
            "route": "api_demo",
            "params": req["params"]
        })
    end

    # GET /*catchall — any path no other route claims: a 404 page, not JSON
    def catchall_demo
        render("home/not_found", {
            "title": "Page not found",
            "layout": "layouts/docs",
            "requested_path": req["path"] || ""
        }, {"status": 404})
    end
end
