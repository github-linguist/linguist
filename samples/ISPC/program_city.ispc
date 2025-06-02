// https://www.shadertoy.com/view/XlsyWB

const float streetDistance = 0.6;
const vec3 streetColor = { 4.0, 8.0, 10.0 };

const float fogDensity = 0.5;
const float fogDistance = 4.0;
const vec3 fogColor = { 0.34, 0.37, 0.4 };

const float windowSize = 0.1;
const float windowDivergence = 0.2;
const vec3 windowColor = { 0.1, 0.2, 0.5 };

const float beaconProb = 0.0003;
const float beaconFreq = 0.6;
const vec3 beaconColor = { 1.5, 0.2, 0.0 };


const float tau = 6.283185;

float hash1(vec2 p2) {
    p2 = fract(p2 * vec2(5.3983, 5.4427));
    p2 += dot(p2.yx, p2.xy + vec2(21.5351, 14.3137));
    return fract(p2.x * p2.y * 95.4337);
}

float hash1(vec2 p2, float p) {
    vec3 p3 = fract(vec3(5.3983 * p2.x, 5.4427 * p2.y, 6.9371 * p));
    p3 += dot(p3, p3.yzx + 19.19);
    return fract((p3.x + p3.y) * p3.z);
}

vec2 hash2(vec2 p2) {
    vec3 p3 = fract(vec3(5.3983 * p2.x, 5.4427 * p2.y, 6.9371 * p2.x));
    p3 += dot(p3, p3.yzx + 19.19);
    return fract((p3.xx + p3.yz) * p3.zy);
}

vec2 hash2(vec2 p2, float p) {
    vec3 p3 = fract(vec3(5.3983 * p2.x, 5.4427 * p2.y, 6.9371 * p));
    p3 += dot(p3, p3.yzx + 19.19);
    return fract((p3.xx + p3.yz) * p3.zy);
}

vec3 hash3(vec2 p2) {
    vec3 p3 = fract(vec3(p2.xyx) * vec3(5.3983, 5.4427, 6.9371));
    p3 += dot(p3, p3.yxz + 19.19);
    return fract((p3.xxy + p3.yzz) * p3.zyx);
}

float noise1(vec2 p) {
    vec2 i = floor(p);
    vec2 f = fract(p);
    vec2 u = f * f * (3.0 - 2.0 * f);
    return mix(mix(hash1(i + vec2(0.0, 0.0)), 
                   hash1(i + vec2(1.0, 0.0)), u.x),
               mix(hash1(i + vec2(0.0, 1.0)), 
                   hash1(i + vec2(1.0, 1.0)), u.x), u.y);
}

vec4 castRay(vec3 eye, vec3 ray) {
    vec2 block = floor(eye.xy);
    vec3 ri = 1.0 / ray;
    vec3 rs = sign(ray);
    vec3 side = 0.5 + 0.5 * rs;
    vec2 ris = ri.xy * rs.xy;
    vec2 dis = (block - eye.xy + 0.5 + rs.xy * 0.5) * ri.xy;
    
    float beacon = 0.0;
    
    for (int i = 0; i < 200; ++i) {
        vec2 lo0 = vec2(block + 0.01);
        vec2 loX = vec2(0.3, 0.3);
        vec2 hi0 = vec2(block + 0.69);
        vec2 hiX = vec2(0.3, 0.3);
        float height = (0.5 + hash1(block)) * (2.0 + 4.0 * pow(noise1(0.1 * block), 2.5));
        
        float dist = 500.0;
        float face = 0.0;
        for (int j = 0; j < 3; ++j) {
            float top = height * (1.0 - 0.1 * float(j));
            vec3 lo = vec3(lo0 + loX * hash2(block, float(j)), 0.0);
            vec3 hi = vec3(hi0 + hiX * hash2(block, float(j) + 0.5), top);

            vec3 wall = mix(hi, lo, side);
            vec3 t = (wall - eye) * ri;

            vec3 dim = step(t.zxy, t) * step(t.yzx, t);            
            float maxT = dot(dim, t);
            float maxFace = 1.0 - dim.z;
            
            vec3 p = eye + maxT * ray;
            dim += step(lo, p) * step(p, hi);
            if (dim.x * dim.y * dim.z > 0.5 && maxT < dist) {
                dist = maxT;
                face = maxFace;
            }
        }
        
        float prob = beaconProb * pow(height, 3.0);
        vec2 h = hash2(block);
        if (h.x < prob) {
            vec3 center = vec3(block + 0.5, height + 0.2);
            float t = dot(center - eye, ray);
            if (t < dist) {
                vec3 p = eye + t * ray;
                float fog = (exp(-p.z / fogDistance) - exp(-eye.z / fogDistance)) / ray.z;
                fog = exp(fogDensity * fog);

                t = distance(center, p);
                fog *= smoothstep(1.0, 0.5, cos(tau * (beaconFreq * iTime + h.y)));
                beacon += fog * pow(clamp(1.0 - 2.0 * t, 0.0, 1.0), 4.0);
            }
        }
        
        if (dist < 400.0) {
            return vec4(dist, beacon, face, 1.0);
        }

        float t = eye.z * ri.z;
        vec3 p = eye - t * ray;
        vec2 g = p.xy - block;
        if (g.x > 0.0 && g.x < 1.0 && g.y > 0.0 && g.y < 1.0) {
            return vec4(-t, beacon, 0.0, 1.0);
        }
        
        vec2 dim = step(dis.xy, dis.yx); 
        dis += dim * ris;
        block += dim * rs.xy;
    }
    
    if (ray.z < 0.0) {
        return vec4(-eye.z * ri.z, beacon, 0.0, 1.0);
    }

    return vec4(0.0, beacon, 0.0, 0.0);
}

inline void mainImage(vec4& fragColor, vec2 fragCoord)
{
    vec2 m = vec2(0.03 * iTime, 0.8);
    if (iMouse.z > 0.0)
        m = iMouse.xy / iResolution.xy;
    m *= tau * vec2(1.0, 0.25);
    
    vec3 center = vec3(6.0 * iTime, 0.5, 3.0);
    float dist = 20.0;
    vec3 eye = center + vec3(dist * sin(m.x) * sin(m.y), dist * cos(m.x) * sin(m.y), dist * cos(m.y));
    float zoom = 3.0;
    
    vec3 forward = normalize(center - eye);
    vec3 right = normalize(cross(forward, vec3(0.0, 0.0, 1.0)));
    vec3 up = cross(right, forward);
    vec2 xy = 2.0 * fragCoord - iResolution.xy;
    zoom *= iResolution.y;
    vec3 ray = normalize(xy.x * right + xy.y * up + zoom * forward);
    
    vec4 res = castRay(eye, ray);
    vec3 p = eye + res.x * ray;

    vec2 block = floor(p.xy);
    vec3 window = floor(p / windowSize);
    float x = hash1(block, window.x);
    float y = hash1(block, window.y);
    float z = hash1(block, window.z);
    vec3 color = windowColor + windowDivergence * (hash3(block) - 0.5);
    color *= smoothstep(0.1, 0.9, fract(2.5 * (x * y * z)));

    vec3 streetLevel = streetColor * exp(-p.z / streetDistance);
    color += streetLevel;
    color = clamp(mix(0.25 * streetLevel, color, res.z), 0.0, 1.0);

    float fog = (exp(-p.z / fogDistance) - exp(-eye.z / fogDistance)) / ray.z;
    fog = exp(fogDensity * fog);
    color = mix(fogColor, color, fog);
    
    color = mix(fogColor, color, res.w);
    color += res.y * beaconColor;
    color += pow(res.y, 2.0);

    fragColor = vec4(color, 1.0);
}
