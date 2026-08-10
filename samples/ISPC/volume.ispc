/*
  Copyright (c) 2011-2023, Intel Corporation

  SPDX-License-Identifier: BSD-3-Clause
*/

typedef float<3> float3;

struct Ray {
    float3 origin, dir;
};


static void
generateRay(const uniform float raster2camera[4][4],
            const uniform float camera2world[4][4],
            float x, float y, Ray &ray) {
    // transform raster coordinate (x, y, 0) to camera space
    float camx = raster2camera[0][0] * x + raster2camera[0][1] * y + raster2camera[0][3];
    float camy = raster2camera[1][0] * x + raster2camera[1][1] * y + raster2camera[1][3];
    float camz = raster2camera[2][3];
    float camw = raster2camera[3][3];
    camx /= camw;
    camy /= camw;
    camz /= camw;

    ray.dir.x = camera2world[0][0] * camx + camera2world[0][1] * camy + camera2world[0][2] * camz;
    ray.dir.y = camera2world[1][0] * camx + camera2world[1][1] * camy + camera2world[1][2] * camz;
    ray.dir.z = camera2world[2][0] * camx + camera2world[2][1] * camy + camera2world[2][2] * camz;

    ray.origin.x = camera2world[0][3] / camera2world[3][3];
    ray.origin.y = camera2world[1][3] / camera2world[3][3];
    ray.origin.z = camera2world[2][3] / camera2world[3][3];
}


static inline bool
Inside(float3 p, float3 pMin, float3 pMax) {
    return (p.x >= pMin.x && p.x <= pMax.x &&
            p.y >= pMin.y && p.y <= pMax.y &&
            p.z >= pMin.z && p.z <= pMax.z);
}


static bool
IntersectP(Ray ray, float3 pMin, float3 pMax, float &hit0, float &hit1) {
    float t0 = -1e30, t1 = 1e30;

    float3 tNear = (pMin - ray.origin) / ray.dir;
    float3 tFar  = (pMax - ray.origin) / ray.dir;
    if (tNear.x > tFar.x) {
        float tmp = tNear.x;
        tNear.x = tFar.x;
        tFar.x = tmp;
    }
    t0 = max(tNear.x, t0);
    t1 = min(tFar.x, t1);

    if (tNear.y > tFar.y) {
        float tmp = tNear.y;
        tNear.y = tFar.y;
        tFar.y = tmp;
    }
    t0 = max(tNear.y, t0);
    t1 = min(tFar.y, t1);

    if (tNear.z > tFar.z) {
        float tmp = tNear.z;
        tNear.z = tFar.z;
        tFar.z = tmp;
    }
    t0 = max(tNear.z, t0);
    t1 = min(tFar.z, t1);

    if (t0 <= t1) {
        hit0 = t0;
        hit1 = t1;
        return true;
    }
    else
        return false;
}


static inline float Lerp(float t, float a, float b) {
    return (1.f - t) * a + t * b;
}


static inline float D(int x, int y, int z, uniform int nVoxels[3],
                      uniform float density[]) {
    x = clamp(x, 0, nVoxels[0]-1);
    y = clamp(y, 0, nVoxels[1]-1);
    z = clamp(z, 0, nVoxels[2]-1);

    #pragma ignore warning(perf)
    return density[z*nVoxels[0]*nVoxels[1] + y*nVoxels[0] + x];
}


static inline float3 Offset(float3 p, float3 pMin, float3 pMax) {
    return (p - pMin) / (pMax - pMin);
}


static float Density(float3 Pobj, float3 pMin, float3 pMax,
                     uniform float density[], uniform int nVoxels[3]) {
    if (!Inside(Pobj, pMin, pMax))
        return 0;
    // Compute voxel coordinates and offsets for _Pobj_
    float3 vox = Offset(Pobj, pMin, pMax);
    vox.x = vox.x * nVoxels[0] - .5f;
    vox.y = vox.y * nVoxels[1] - .5f;
    vox.z = vox.z * nVoxels[2] - .5f;
    int vx = (int)(vox.x), vy = (int)(vox.y), vz = (int)(vox.z);
    float dx = vox.x - vx, dy = vox.y - vy, dz = vox.z - vz;

    // Trilinearly interpolate density values to compute local density
    float d00 = Lerp(dx, D(vx, vy, vz, nVoxels, density),
                     D(vx+1, vy, vz, nVoxels, density));
    float d10 = Lerp(dx, D(vx, vy+1, vz, nVoxels, density),
                     D(vx+1, vy+1, vz, nVoxels, density));
    float d01 = Lerp(dx, D(vx, vy, vz+1, nVoxels, density),
                     D(vx+1, vy, vz+1, nVoxels, density));
    float d11 = Lerp(dx, D(vx, vy+1, vz+1, nVoxels, density),
                     D(vx+1, vy+1, vz+1, nVoxels, density));
    float d0 = Lerp(dy, d00, d10);
    float d1 = Lerp(dy, d01, d11);
    return Lerp(dz, d0, d1);
}


/* Returns the transmittance between two points p0 and p1, in a volume
   with extent (pMin,pMax) with transmittance coefficient sigma_t,
   defined by nVoxels[3] voxels in each dimension in the given density
   array. */
static float
transmittance(uniform float3 p0, float3 p1, uniform float3 pMin,
              uniform float3 pMax, uniform float sigma_t,
              uniform float density[], uniform int nVoxels[3]) {
    float rayT0, rayT1;
    Ray ray;
    ray.origin = p1;
    ray.dir = p0 - p1;

    // Find the parametric t range along the ray that is inside the volume.
    if (!IntersectP(ray, pMin, pMax, rayT0, rayT1))
        return 1.;

    rayT0 = max(rayT0, 0.f);

    // Accumulate beam transmittance in tau
    float tau = 0;
    float rayLength = sqrt(ray.dir.x * ray.dir.x + ray.dir.y * ray.dir.y +
                           ray.dir.z * ray.dir.z);
    uniform float stepDist = 0.2;
    float stepT = stepDist / rayLength;

    float t = rayT0;
    float3 pos = ray.origin + ray.dir * rayT0;
    float3 dirStep = ray.dir * stepT;
    while (t < rayT1) {
        tau += stepDist * sigma_t * Density(pos, pMin, pMax, density, nVoxels);
        pos = pos + dirStep;
        t += stepT;
    }

    return exp(-tau);
}


static inline float
distanceSquared(float3 a, float3 b) {
    float3 d = a-b;
    return d.x*d.x + d.y*d.y + d.z*d.z;
}


static float
raymarch(uniform float density[], uniform int nVoxels[3], Ray ray) {
    float rayT0, rayT1;
    uniform float3 pMin = {.3, -.2, .3}, pMax = {1.8, 2.3, 1.8};
    uniform float3 lightPos = { -1, 4, 1.5 };

    cif (!IntersectP(ray, pMin, pMax, rayT0, rayT1))
        return 0.;

    rayT0 = max(rayT0, 0.f);

    // Parameters that define the volume scattering characteristics and
    // sampling rate for raymarching
    uniform float Le = .25;            // Emission coefficient
    uniform float sigma_a = 10;        // Absorption coefficient
    uniform float sigma_s = 10;        // Scattering coefficient
    uniform float stepDist = 0.025;    // Ray step amount
    uniform float lightIntensity = 40; // Light source intensity

    float tau = 0.f;  // accumulated beam transmittance
    float L = 0;      // radiance along the ray
    float rayLength = sqrt(ray.dir.x * ray.dir.x + ray.dir.y * ray.dir.y +
                           ray.dir.z * ray.dir.z);
    float stepT = stepDist / rayLength;

    float t = rayT0;
    float3 pos = ray.origin + ray.dir * rayT0;
    float3 dirStep = ray.dir * stepT;
    cwhile (t < rayT1) {
        float d = Density(pos, pMin, pMax, density, nVoxels);

        // terminate once attenuation is high
        float atten = exp(-tau);
        if (atten < .005)
            break;

        // direct lighting
        float Li = lightIntensity / distanceSquared(lightPos, pos) *
            transmittance(lightPos, pos, pMin, pMax, sigma_a + sigma_s,
                          density, nVoxels);
        L += stepDist * atten * d * sigma_s * (Li + Le);

        // update beam transmittance
        tau += stepDist * (sigma_a + sigma_s) * d;

        pos = pos + dirStep;
        t += stepT;
    }

    // Gamma correction
    return pow(L, 1.f / 2.2f);
}


/* Utility routine used by both the task-based and the single-core entrypoints.
   Renders a tile of the image, covering [x0,x0) * [y0, y1), storing the
   result into the image[] array.
 */
static void
volume_tile(uniform int x0, uniform int y0, uniform int x1,
            uniform int y1, uniform float density[], uniform int nVoxels[3],
            const uniform float raster2camera[4][4],
            const uniform float camera2world[4][4],
            uniform int width, uniform int height, uniform float image[]) {
    // Work on 4x4=16 pixel big tiles of the image.  This function thus
    // implicitly assumes that both (x1-x0) and (y1-y0) are evenly divisble
    // by 4.
    for (uniform int y = y0; y < y1; y += 4) {
        for (uniform int x = x0; x < x1; x += 4) {
            foreach (o = 0 ... 16) {
                // These two arrays encode the mapping from [0,15] to
                // offsets within the 4x4 pixel block so that we render
                // each pixel inside the block
                const uniform int xoffsets[16] = { 0, 1, 0, 1, 2, 3, 2, 3,
                                                   0, 1, 0, 1, 2, 3, 2, 3 };
                const uniform int yoffsets[16] = { 0, 0, 1, 1, 0, 0, 1, 1,
                                                   2, 2, 3, 3, 2, 2, 3, 3 };

                // Figure out the pixel to render for this program instance
                int xo = x + xoffsets[o], yo = y + yoffsets[o];

                // Use viewing parameters to compute the corresponding ray
                // for the pixel
                Ray ray;
                generateRay(raster2camera, camera2world, xo, yo, ray);

                // And raymarch through the volume to compute the pixel's
                // value
                int offset = yo * width + xo;
                #pragma ignore warning(perf)
                image[offset] = raymarch(density, nVoxels, ray);
            }
        }
    }
}


task void
volume_task(uniform float density[], uniform int nVoxels[3],
            const uniform float raster2camera[4][4],
            const uniform float camera2world[4][4],
            uniform int width, uniform int height, uniform float image[]) {
    uniform int dx = 8, dy = 8; // must match value in volume_ispc_tasks
    uniform int xbuckets = (width + (dx-1)) / dx;
    uniform int ybuckets = (height + (dy-1)) / dy;

    uniform int x0 = (taskIndex % xbuckets) * dx;
    uniform int y0 = (taskIndex / xbuckets) * dy;
    uniform int x1 = x0 + dx, y1 = y0 + dy;
    x1 = min(x1, width);
    y1 = min(y1, height);

    volume_tile(x0, y0, x1, y1, density, nVoxels, raster2camera,
                 camera2world, width, height, image);
}


export void
volume_ispc(uniform float density[], uniform int nVoxels[3],
            const uniform float raster2camera[4][4],
            const uniform float camera2world[4][4],
            uniform int width, uniform int height, uniform float image[]) {
    volume_tile(0, 0, width, height, density, nVoxels, raster2camera,
                camera2world, width, height,  image);
}


export void
volume_ispc_tasks(uniform float density[], uniform int nVoxels[3],
                  const uniform float raster2camera[4][4],
                  const uniform float camera2world[4][4],
                  uniform int width, uniform int height, uniform float image[]) {
    // Launch tasks to work on (dx,dy)-sized tiles of the image
    uniform int dx = 8, dy = 8;
    uniform int nTasks = ((width+(dx-1))/dx) * ((height+(dy-1))/dy);
    launch[nTasks] volume_task(density, nVoxels, raster2camera, camera2world,
                               width, height, image);
}