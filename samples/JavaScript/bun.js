#!/usr/bin/env bun


const db = new Map();
let idCounter = 1;

console.log("success: server running on port 3000");

export default Bun.serve({
  port: 3000,
  async fetch(req) {
    const url = new URL(req.url);
    const path = url.pathname;
    const method = req.method;


    if (path === "/items" && method === "GET") {
      return Response.json(Array.from(db.values()));
    }


    if (path === "/items" && method === "POST") {
      const body = await req.json();
      if (!body.name) return new Response("Name required", { status: 400 });
      
      const newItem = { id: idCounter++, name: body.name, createdAt: new Date() };
      db.set(newItem.id, newItem);
      return Response.json(newItem, { status: 201 });
    }


    const itemMatch = path.match(/^\/items\/(\d+)$/);
    if (itemMatch && method === "GET") {
      const id = parseInt(itemMatch[1]);
      const item = db.get(id);
      if (!item) return new Response("Not Found", { status: 404 });
      return Response.json(item);

    if (itemMatch && method === "DELETE") {
      const id = parseInt(itemMatch[1]);
      if (!db.has(id)) return new Response("Not Found", { status: 404 });
      db.delete(id);
      return new Response(null, { status: 204 });
    }

    return new Response("Not Found", { status: 404 });
  },
});
