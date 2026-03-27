#!/usr/bin/env -S deno run

const PORT = 8080;
const rateLimits = new Map(); 

function calculateComplexMetrics(data) {
  if (!Array.isArray(data) || data.length === 0) return { error: "Invalid data" };
  
  const sum = data.reduce((a, b) => a + b, 0);
  const avg = sum / data.length;
  const max = Math.max(...data);
  const complexityScore = (sum * avg) / (max || 1);
  
  return {
    count: data.length,
    average: parseFloat(avg.toFixed(2)),
    max,
    complexityScore: parseFloat(complexityScore.toFixed(2)),
    timestamp: new Date().toISOString()
  };
}


async function handler(request) {
  const ip = request.headers.get("x-forwarded-for") || "unknown";
  
  
  if (rateLimits.has(ip) && (Date.now() - rateLimits.get(ip) < 1000)) {
    return new Response("Too Many Requests", { status: 429 });
  }
  rateLimits.set(ip, Date.now());

  
  if (request.method === "POST") {
    try {
      const body = await request.json();
      const result = calculateComplexMetrics(body.numbers || []);
      
      return new Response(JSON.stringify(result), {
        status: 200,
        headers: { "Content-Type": "application/json" },
      });
    } catch (e) {
      return new Response(JSON.stringify({ error: "Invalid JSON" }), { status: 400 });
    }
  }

  return new Response("Send a POST request with { 'numbers': [1,2,3] }", { status: 200 });
}


console.log(`Server running on http://localhost:${PORT}`);
Deno.serve({ port: PORT }, handler);
