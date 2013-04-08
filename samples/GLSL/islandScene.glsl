//// High quality (Some browsers may freeze or crash)
//#define HIGHQUALITY

//// Medium quality (Should be fine on all systems, works on Intel HD2000 on Win7 but quite slow)
//#define MEDIUMQUALITY

//// Defaults
//#define REFLECTIONS
#define SHADOWS
//#define GRASS
//#define SMALL_WAVES
#define RAGGED_LEAVES
//#define DETAILED_NOISE
//#define LIGHT_AA // 2 sample SSAA
//#define HEAVY_AA // 2x2 RG SSAA
//#define TONEMAP

//// Configurations
#ifdef MEDIUMQUALITY
	#define SHADOWS
	#define SMALL_WAVES
	#define RAGGED_LEAVES
	#define TONEMAP
#endif

#ifdef HIGHQUALITY
	#define REFLECTIONS
	#define SHADOWS
	//#define GRASS
	#define SMALL_WAVES
	#define RAGGED_LEAVES
	#define DETAILED_NOISE
	#define LIGHT_AA
	#define TONEMAP
#endif

// Constants
const float eps = 1e-5;
const float PI = 3.14159265359;

const vec3 sunDir = vec3(0.79057,-0.47434, 0.0);
const vec3 skyCol = vec3(0.3, 0.5, 0.8);
const vec3 sandCol = vec3(0.9, 0.8, 0.5);
const vec3 treeCol = vec3(0.8, 0.65, 0.3);
const vec3 grassCol = vec3(0.4, 0.5, 0.18);
const vec3 leavesCol = vec3(0.3, 0.6, 0.2);
const vec3 leavesPos = vec3(-5.1,13.4, 0.0);

#ifdef TONEMAP
const vec3 sunCol = vec3(1.8, 1.7, 1.6);
#else
const vec3 sunCol = vec3(0.9, 0.85, 0.8);
#endif

const float exposure = 1.1; // Only used when tonemapping

// Description : Array and textureless GLSL 2D/3D/4D simplex
// noise functions.
// Author : Ian McEwan, Ashima Arts.
// License : Copyright (C) 2011 Ashima Arts. All rights reserved.
// Distributed under the MIT License. See LICENSE file.
// https://github.com/ashima/webgl-noise
vec3 mod289(vec3 x) {
  return x - floor(x * (1.0 / 289.0)) * 289.0;
}

vec4 mod289(vec4 x) {
  return x - floor(x * (1.0 / 289.0)) * 289.0;
}

vec4 permute(vec4 x) {
     return mod289(((x*34.0)+1.0)*x);
}

vec4 taylorInvSqrt(vec4 r) {
  return 1.79284291400159 - 0.85373472095314 * r;
}

float snoise(vec3 v) {
  const vec2 C = vec2(1.0/6.0, 1.0/3.0) ;
  const vec4 D = vec4(0.0, 0.5, 1.0, 2.0);

// First corner
  vec3 i = floor(v + dot(v, C.yyy) );
  vec3 x0 = v - i + dot(i, C.xxx) ;

// Other corners
  vec3 g = step(x0.yzx, x0.xyz);
  vec3 l = 1.0 - g;
  vec3 i1 = min( g.xyz, l.zxy );
  vec3 i2 = max( g.xyz, l.zxy );

  // x0 = x0 - 0.0 + 0.0 * C.xxx;
  // x1 = x0 - i1 + 1.0 * C.xxx;
  // x2 = x0 - i2 + 2.0 * C.xxx;
  // x3 = x0 - 1.0 + 3.0 * C.xxx;
  vec3 x1 = x0 - i1 + C.xxx;
  vec3 x2 = x0 - i2 + C.yyy; // 2.0*C.x = 1/3 = C.y
  vec3 x3 = x0 - D.yyy; // -1.0+3.0*C.x = -0.5 = -D.y

// Permutations
  i = mod289(i);
  vec4 p = permute( permute( permute(
             i.z + vec4(0.0, i1.z, i2.z, 1.0 ))
           + i.y + vec4(0.0, i1.y, i2.y, 1.0 ))
           + i.x + vec4(0.0, i1.x, i2.x, 1.0 ));

// Gradients: 7x7 points over a square, mapped onto an octahedron.
// The ring size 17*17 = 289 is close to a multiple of 49 (49*6 = 294)
  float n_ = 0.142857142857; // 1.0/7.0
  vec3 ns = n_ * D.wyz - D.xzx;

  vec4 j = p - 49.0 * floor(p * ns.z * ns.z); // mod(p,7*7)

  vec4 x_ = floor(j * ns.z);
  vec4 y_ = floor(j - 7.0 * x_ ); // mod(j,N)

  vec4 x = x_ *ns.x + ns.yyyy;
  vec4 y = y_ *ns.x + ns.yyyy;
  vec4 h = 1.0 - abs(x) - abs(y);

  vec4 b0 = vec4( x.xy, y.xy );
  vec4 b1 = vec4( x.zw, y.zw );

  //vec4 s0 = vec4(lessThan(b0,0.0))*2.0 - 1.0;
  //vec4 s1 = vec4(lessThan(b1,0.0))*2.0 - 1.0;
  vec4 s0 = floor(b0)*2.0 + 1.0;
  vec4 s1 = floor(b1)*2.0 + 1.0;
  vec4 sh = -step(h, vec4(0.0));

  vec4 a0 = b0.xzyw + s0.xzyw*sh.xxyy ;
  vec4 a1 = b1.xzyw + s1.xzyw*sh.zzww ;

  vec3 p0 = vec3(a0.xy,h.x);
  vec3 p1 = vec3(a0.zw,h.y);
  vec3 p2 = vec3(a1.xy,h.z);
  vec3 p3 = vec3(a1.zw,h.w);

//Normalise gradients
  vec4 norm = taylorInvSqrt(vec4(dot(p0,p0), dot(p1,p1), dot(p2, p2), dot(p3,p3)));
  p0 *= norm.x;
  p1 *= norm.y;
  p2 *= norm.z;
  p3 *= norm.w;

// Mix final noise value
  vec4 m = max(0.6 - vec4(dot(x0,x0), dot(x1,x1), dot(x2,x2), dot(x3,x3)), 0.0);
  m = m * m;
  return 42.0 * dot( m*m, vec4( dot(p0,x0), dot(p1,x1),
                                dot(p2,x2), dot(p3,x3) ) );
}



// Main
float fbm(vec3 p)
{
	float final = snoise(p); 
	p *= 1.94; final += snoise(p) * 0.5;
	#ifdef DETAILED_NOISE
	p *= 3.75; final += snoise(p) * 0.25;
	return final / 1.75;
	#else
	return final / 1.5;
	#endif
}

float waterHeight(vec3 p)
{
	float d = length(p.xz);
	float h = sin(d * 1.5 + iGlobalTime * 3.0) * 12.0 / d; // Island waves
	#ifdef SMALL_WAVES
	h += fbm(p*0.5); // Other waves
	#endif
	return h;
}

vec3 bump(vec3 pos, vec3 rayDir)
{
	float s = 2.0;
	
	// Fade out waves to reduce aliasing
	float dist = dot(pos, rayDir);
	s *= dist < 2.0 ? 1.0 : 1.4142 / sqrt(dist);
	
	// Calculate normal from heightmap
	vec2 e = vec2(1e-2, 0.0);
	vec3 p = vec3(pos.x, iGlobalTime*0.5, pos.z)*0.7;
	float m = waterHeight(p)*s;
	return normalize(vec3(
		waterHeight(p+e.xyy)*s-m,
		1.0,
		waterHeight(p+e.yxy)*s-m
	));
}

// Ray intersections
vec4 intersectSphere(vec3 rpos, vec3 rdir, vec3 pos, float rad)
{
	vec3 op = pos - rpos;
	float b = dot(op, rdir); 
	float det = b*b - dot(op, op) + rad*rad; 
		
	if (det > 0.0)
	{
		det = sqrt(det);
		float t = b - det;
		if (t > eps)
			return vec4(-normalize(rpos+rdir*t-pos), t);
	}
	
	return vec4(0.0);
}

vec4 intersectCylinder(vec3 rpos, vec3 rdir, vec3 pos, float rad)
{
	vec3 op = pos - rpos;
	vec2 rdir2 = normalize(rdir.yz);
	float b = dot(op.yz, rdir2);
	float det = b*b - dot(op.yz, op.yz) + rad*rad; 
	
	if (det > 0.0)
	{
		det = sqrt(det);
		float t = b - det;
		if (t > eps)
			return vec4(-normalize(rpos.yz+rdir2*t-pos.yz), 0.0, t);
		t = b + det;
		if (t > eps)
			return vec4(-normalize(rpos.yz+rdir2*t-pos.yz), 0.0, t);
	}
	
	return vec4(0.0);
}

vec4 intersectPlane(vec3 rayPos, vec3 rayDir, vec3 n, float d)
{
	float t = -(dot(rayPos, n) + d) / dot(rayDir, n);
	return vec4(n * sign(dot(rayDir, n)), t);
}

// Helper functions
vec3 rotate(vec3 p, float theta)
{
	float c = cos(theta), s = sin(theta);
	return vec3(p.x * c + p.z * s, p.y,
				p.z * c - p.x * s);
}

float impulse(float k, float x) // by iq
{
    float h = k*x;
    return h * exp(1.0 - h);
}

// Raymarched parts of scene
float grass(vec3 pos)
{
	float h = length(pos - vec3(0.0, -7.0, 0.0)) - 8.0;
	
	if (h > 2.0) return h; // Optimization (Avoid noise if too far away)
	
	return h + snoise(pos * 3.0) * 0.1 + pos.y * 0.9;
}

float tree(vec3 pos)
{
	pos.y -= 0.5;
	float s = sin(pos.y*0.03);
	float c = cos(pos.y*0.03);
	mat2 m = mat2(c, -s, s, c);
	vec3 p = vec3(m*pos.xy, pos.z);
	
	float width = 1.0 - pos.y * 0.02 - clamp(sin(pos.y * 8.0) * 0.1, 0.05, 0.1);
	
	return max(length(p.xz) - width, pos.y - 12.5);
}

vec2 scene(vec3 pos)
{
	float vtree = tree(pos);
	#ifdef GRASS
	float vgrass = grass(pos);
	float v = min(vtree, vgrass);
	#else
	float v = vtree;
	#endif
	return vec2(v, v == vtree ? 2.0 : 1.0);
}

vec3 normal(vec3 pos)
{
	vec2 eps = vec2(1e-3, 0.0);
	float h = scene(pos).x;
	return normalize(vec3(
		scene(pos-eps.xyy).x-h,
		scene(pos-eps.yxy).x-h,
		scene(pos-eps.yyx).x-h
	));
}

float plantsShadow(vec3 rayPos, vec3 rayDir)
{
	// Soft shadow taken from iq
	float k = 6.0;
	float t = 0.0;
	float s = 1.0;	
	for (int i = 0; i < 30; i++)
	{
		vec3 pos = rayPos+rayDir*t;	
		vec2 res = scene(pos);		
		if (res.x < 0.001) return 0.0;
		s = min(s, k*res.x/t); 
		t += max(res.x, 0.01);
	}
	
	return s*s*(3.0 - 2.0*s);
}

// Ray-traced parts of scene
vec4 intersectWater(vec3 rayPos, vec3 rayDir)
{
	float h = sin(20.5 + iGlobalTime * 2.0) * 0.03;
	float t = -(rayPos.y + 2.5 + h) / rayDir.y;
	return vec4(0.0, 1.0, 0.0, t);
}

vec4 intersectSand(vec3 rayPos, vec3 rayDir)
{
	return intersectSphere(rayPos, rayDir, vec3(0.0,-24.1,0.0), 24.1);
}

vec4 intersectTreasure(vec3 rayPos, vec3 rayDir)
{
	return vec4(0.0);
}

vec4 intersectLeaf(vec3 rayPos, vec3 rayDir, float openAmount)
{	
	vec3 dir = normalize(vec3(0.0, 1.0, openAmount));
	float offset = 0.0;
			
	vec4 res = intersectPlane(rayPos, rayDir, dir, 0.0);
	vec3 pos = rayPos+rayDir*res.w;
	#ifdef RAGGED_LEAVES
	offset = snoise(pos*0.8) * 0.3;
	#endif
	if (pos.y > 0.0 || length(pos * vec3(0.9, 2.0, 1.0)) > 4.0 - offset) res.w = 0.0;
	
	vec4 res2 = intersectPlane(rayPos, rayDir, vec3(dir.xy, -dir.z), 0.0);
	pos = rayPos+rayDir*res2.w;
	#ifdef RAGGED_LEAVES
	offset = snoise(pos*0.8) * 0.3;
	#endif
	if (pos.y > 0.0 || length(pos * vec3(0.9, 2.0, 1.0)) > 4.0 - offset) res2.w = 0.0;
	
	if (res2.w > 0.0 && res2.w < res.w || res.w <= 0.0)
		res = res2;
		
	return res;
}

vec4 leaves(vec3 rayPos, vec3 rayDir)
{
	float t = 1e20;
	vec3 n = vec3(0.0);
	
	rayPos -= leavesPos;
	
	float sway = impulse(15.0, fract(iGlobalTime / PI * 0.125));
	float upDownSway = sway * -sin(iGlobalTime) * 0.06;
	float openAmount = sway * max(-cos(iGlobalTime) * 0.4, 0.0);
	
	float angleOffset = -0.1;	
	for (float k = 0.0; k < 6.2; k += 0.75)
	{
		// Left-right
		float alpha = k + (k - PI) * sway * 0.015;
		vec3 p = rotate(rayPos, alpha);
		vec3 d = rotate(rayDir, alpha);
		
		// Up-down
		angleOffset *= -1.0;
		float theta = -0.4 + 
			angleOffset + 
			cos(k) * 0.35 + 
			upDownSway + 
			sin(iGlobalTime+k*10.0) * 0.03 * (sway + 0.2);
		
		p = rotate(p.xzy, theta).xzy;
		d = rotate(d.xzy, theta).xzy;
	
		// Shift
		p -= vec3(5.4, 0.0, 0.0);
		
		// Intersect individual leaf
		vec4 res = intersectLeaf(p, d, 1.0+openAmount);
		if (res.w > 0.0 && res.w < t)
		{
			t = res.w;
			n = res.xyz;
		}
	}
	
	return vec4(n, t);
}

// Lighting
float shadow(vec3 rayPos, vec3 rayDir)
{	
	float s = 1.0;
	
	// Intersect sand
	//vec4 resSand = intersectSand(rayPos, rayDir);
	//if (resSand.w > 0.0) return 0.0;
	
	// Intersect plants
	s = min(s, plantsShadow(rayPos, rayDir));
	if (s < 0.0001) return 0.0;
	
	// Intersect leaves
	vec4 resLeaves = leaves(rayPos, rayDir);
	if (resLeaves.w > 0.0 && resLeaves.w < 1e7) return 0.0;
	
	return s;
}

vec3 light(vec3 p, vec3 n)
{
	float s = 1.0;
	
	#ifdef SHADOWS
	s = shadow(p-sunDir*0.01, -sunDir);
	#endif
	
	vec3 col = sunCol * min(max(dot(n, sunDir), 0.0), s);
	col += skyCol * (-n.y * 0.5 + 0.5) * 0.3;
	return col;
}

vec3 lightLeaves(vec3 p, vec3 n)
{
	float s = 1.0;
	
	#ifdef SHADOWS
	s = shadow(p-sunDir*0.01, -sunDir);
	#endif
	
	float ao = min(length(p - leavesPos) * 0.1, 1.0);
	
	float ns = dot(n, sunDir);
	float d = sqrt(max(ns, 0.0));
	vec3 col = sunCol * min(d, s);
	col += sunCol * max(-ns, 0.0) * vec3(0.3, 0.3, 0.1) * ao;
	col += skyCol * (-n.y * 0.5 + 0.5) * 0.3 * ao;
	return col;
}

vec3 sky(vec3 n)
{
	return skyCol * (1.0 - n.y * 0.8);
}

// Ray-marching
vec4 plants(vec3 rayPos, vec3 rayDir)
{
	float t = 0.0;
	
	for (int i = 0; i < 40; i++)
	{
		vec3 pos = rayPos+rayDir*t;	
		vec2 res = scene(pos);
		float h = res.x;
		
		if (h < 0.001)
		{
			vec3 col = res.y == 2.0 ? treeCol : grassCol;
			float uvFact = res.y == 2.0 ? 1.0 : 10.0;
			
			vec3 n = normal(pos);
			vec2 uv = vec2(n.x, pos.y * 0.5) * 0.2 * uvFact;
			vec3 tex = texture2D(iChannel0, uv).rgb * 0.6 + 0.4;
			float ao = min(length(pos - leavesPos) * 0.1, 1.0);
			return vec4(col * light(pos, n) * ao * tex, t);
		}
		
		t += h;
	}
	
	return vec4(sky(rayDir), 1e8);
}

// Final combination
vec3 traceReflection(vec3 rayPos, vec3 rayDir)
{
	vec3 col = vec3(0.0);
	float t = 1e20;
			
	// Intersect plants
	vec4 resPlants = plants(rayPos, rayDir);
	if (resPlants.w > 0.0 && resPlants.w < t)
	{
		t = resPlants.w;
		col = resPlants.xyz;
	}
	
	// Intersect leaves
	vec4 resLeaves = leaves(rayPos, rayDir);
	if (resLeaves.w > 0.0 && resLeaves.w < t)
	{
		vec3 pos = rayPos + rayDir * resLeaves.w;
		vec2 uv = (pos.xz - leavesPos.xz) * 0.3;
		float tex = texture2D(iChannel0, uv).r * 0.6 + 0.5;
		
		t = resLeaves.w;
		col = leavesCol * lightLeaves(pos, resLeaves.xyz) * tex;
	}
		
	if (t > 1e7) return sky(rayDir);
	
	return col;
}

vec3 trace(vec3 rayPos, vec3 rayDir)
{
	vec3 col = vec3(0.0);
	float t = 1e20;
	
	// Intersect sand
	vec4 resSand = intersectSand(rayPos, rayDir);
	if (resSand.w > 0.0)
	{
		vec3 pos = rayPos + rayDir * resSand.w;
		t = resSand.w;

		col = sandCol * light(pos, resSand.xyz);
	}
	
	// Intersect treasure chest
	vec4 resTreasure = intersectTreasure(rayPos, rayDir);
	if (resTreasure.w > 0.0 && resTreasure.w < t)
	{
		vec3 pos = rayPos + rayDir * resTreasure.w;
		t = resTreasure.w;
		col = leavesCol * light(pos, resTreasure.xyz);
	}
	
	// Intersect leaves
	vec4 resLeaves = leaves(rayPos, rayDir);
	if (resLeaves.w > 0.0 && resLeaves.w < t)
	{
		vec3 pos = rayPos + rayDir * resLeaves.w;
		vec2 uv = (pos.xz - leavesPos.xz) * 0.3;
		float tex = texture2D(iChannel0, uv).r * 0.6 + 0.5;
		
		t = resLeaves.w;
		col = leavesCol * lightLeaves(pos, resLeaves.xyz) * tex;
	}
	
	// Intersect plants
	vec4 resPlants = plants(rayPos, rayDir);
	if (resPlants.w > 0.0 && resPlants.w < t)
	{
		t = resPlants.w;
		col = resPlants.xyz;
	}
		
	// Intersect water	
	vec4 resWater = intersectWater(rayPos, rayDir);
	if (resWater.w > 0.0 && resWater.w < t)
	{
		vec3 pos = rayPos + rayDir * resWater.w;
		float dist = t - resWater.w;
		vec3 n = bump(pos, rayDir);
		
		float ct = -min(dot(n,rayDir), 0.0);
		float fresnel = 0.9 - 0.9 * pow(1.0 - ct, 5.0);
		
		vec3 trans = col * exp(-dist * vec3(1.0, 0.7, 0.4) * 3.0);
		vec3 reflDir = normalize(reflect(rayDir, n));
		vec3 refl = sky(reflDir);
		
		#ifdef REFLECTIONS
		if (dot(pos, rayDir) < -2.0)
			refl = traceReflection(pos, reflDir).rgb;
		#endif
				
		t = resWater.t;
		col = mix(refl, trans, fresnel);
	}
	
	if (t > 1e7) return sky(rayDir);
	
	return col;
}

// Ray-generation
vec3 camera(vec2 px)
{
	vec2 rd = (px / iResolution.yy - vec2(iResolution.x/iResolution.y*0.5-0.5, 0.0)) * 2.0 - 1.0;
	float t = sin(iGlobalTime * 0.1) * 0.2;
	vec3 rayDir = normalize(vec3(rd.x, rd.y, 1.0));
	vec3 rayPos = vec3(0.0, 3.0, -18.0);
	return trace(rayPos, rayDir);
}

void main(void)
{
	#ifdef HEAVY_AA
		vec3 col = camera(gl_FragCoord.xy+vec2(0.0,0.5))*0.25;
		col += camera(gl_FragCoord.xy+vec2(0.25,0.0))*0.25;
		col += camera(gl_FragCoord.xy+vec2(0.5,0.75))*0.25;
		col += camera(gl_FragCoord.xy+vec2(0.75,0.25))*0.25;
	#else
		vec3 col = camera(gl_FragCoord.xy);
		#ifdef LIGHT_AA
			col = col * 0.5 + camera(gl_FragCoord.xy+vec2(0.5,0.5))*0.5;
		#endif
	#endif
	
	#ifdef TONEMAP
	// Optimized Haarm-Peter Duiker’s curve
	vec3 x = max(vec3(0.0),col*exposure-0.004);
	col = (x*(6.2*x+.5))/(x*(6.2*x+1.7)+0.06);
	#else
	col = pow(col, vec3(0.4545));
	#endif
	
	gl_FragColor = vec4(col, 1.0);
}