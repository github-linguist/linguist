#!/usr/bin/env bun


/**
 * @author oopsio
 * @copyright (c) 2026-present oopsio
 * @license MIT
 */

/*
  Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
  The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

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
