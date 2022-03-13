#include <stdio.h>
#include <cstdlib>
#include <cstdio>
#include <cmath>
#include <fstream>
#include <vector>
#include <iostream>
#include <cassert>
#include <chrono>
#include <cuda.h>
////////////////TOO MANY LIBS 


#if defined __linux__ || defined __APPLE__
// "Compiled for Linux
#else
// Windows doesn't define these values by default, Linux does
#define M_PI 3.141592653589793
#define INFINITY 1e8
#endif

#define MAX_RAY_DEPTH 5
//width and height of image in render

struct vect3f {
	float x, y, z;
};
/*
class Sphere
{
public:
    vect3f center;                           /// position of the sphere
    float radius, radius2;                  /// sphere radius and radius^2
    vect3f surfaceColor, emissionColor;      /// surface color and emission (light)
    float transparency, reflection;         /// surface transparency and reflectivity
    Sphere(
        const vect3f &c,
        const float &r,
        const vect3f &sc,
        const float &refl,
        const float &transp,
        const vect3f &ec) :
        center(c), radius(r), radius2(r * r), surfaceColor(sc), emissionColor(ec),
        transparency(transp), reflection(refl)
    { /* empty }
};
*/
struct Sphere {
    vect3f center;                           /// position of the sphere
    float radius, radius2;                  /// sphere radius and radius^2
    vect3f surfaceColor;      /// surface color and emission (light)
    float transparency, reflection;  
    vect3f emissionColor;
};


__device__ float dotp (vect3f A, vect3f B) {
	return A.x*B.x + A.y*B.y + A.z*B.z;
}

__device__ bool intersect(const vect3f &rayorig, const vect3f &raydir, float &t0, float &t1, Sphere S) {
	vect3f l = {S.center.x - rayorig.x, S.center.y - rayorig.y, S.center.z - rayorig.z};
    float tca = dotp(l, raydir);
    if (tca < 0) return false;
    float d2 = dotp(l, l) - tca * tca;
    if (d2 > S.radius2) return false;
    float thc = sqrt(S.radius2 - d2);
    t0 = tca - thc;
    t1 = tca + thc;
    
    return true;
}

__device__ void vect3f_normalize(vect3f *V) {
	vect3f temp = *V;
	float norm = (temp.x*temp.x) + (temp.y*temp.y) + (temp.z*temp.z);
	if (norm > 0) {
		float invnorm = 1/sqrt(norm);
		temp.x *= invnorm;
		temp.y *= invnorm;
		temp.z *= invnorm;
		*V = temp;
	}
}

__device__ float mix(const float &a, const float &b, const float &mix) {
    return b * mix + a * (1 - mix);
}

__device__ vect3f trace(const vect3f &rayorig,  const vect3f &raydir, Sphere* &spheres,
    const int &depth) {
    //if (raydir.length() != 1) std::cerr << "Error " << raydir << std::endl;
    float tnear = INFINITY;
    const Sphere* sphere = NULL;
    // find intersection of this ray with the sphere in the scene
    for (unsigned i = 0; i < sizeof(spheres); ++i) {
        float t0 = INFINITY, t1 = INFINITY;
        if (intersect(rayorig, raydir, t0, t1, spheres[i])) {
            if (t0 < 0) t0 = t1;
            if (t0 < tnear) {
                tnear = t0;
                sphere = &spheres[i];
            }
        }
    }
	
    if (!sphere) return vect3f({2, 0, 0}); /////////////////////////////////////////////////////
    
    vect3f surfaceColor = {0, 0, 0}; // color of the ray/surfaceof the object intersected by the ray
    vect3f phit;
    phit.x = rayorig.x + raydir.x * tnear; // point of intersection
    phit.y = rayorig.y + raydir.y * tnear; // point of intersection
    phit.z = rayorig.z + raydir.z * tnear; // point of intersection

    vect3f nhit = {phit.x - sphere->center.x, phit.y - sphere->center.y, phit.z - sphere->center.z}; // normal at the intersection point
    vect3f_normalize(&nhit); // normalize normal direction

    float bias = 1e-4; // add some bias to the point from which we will be tracing
    bool inside = false;
    if (dotp(raydir, nhit) > 0) {
    	nhit = {-nhit.x, -nhit.y, -nhit.z};
    	inside = true;
   }
   
    if ((sphere->transparency > 0 || sphere->reflection > 0) && depth < MAX_RAY_DEPTH) {
        float facingratio = -dotp(raydir, nhit);
        // change the mix value to tweak the effect
        float fresneleffect = mix(pow(1 - facingratio, 3), 1, 0.1);

        vect3f refldir; 
        refldir.x = raydir.x - nhit.x * 2 * dotp(raydir, nhit);
        refldir.y = raydir.y - nhit.y * 2 * dotp(raydir, nhit);
        refldir.z = raydir.z - nhit.z * 2 * dotp(raydir, nhit);
        vect3f_normalize(&refldir);
        
        vect3f refl_temp;
        refl_temp.x = phit.x + nhit.x*bias;
        refl_temp.y = phit.y + nhit.y*bias;
        refl_temp.z = phit.z + nhit.z*bias;
        
        vect3f reflection = trace(refl_temp, refldir, spheres, depth + 1);
        vect3f refraction = {0, 0, 0};
        // if the sphere is also transparent compute refraction ray (transmission)
        if (sphere->transparency) {
            float ior = 1.1, eta = (inside) ? ior : 1 / ior; // are we inside or outside the surface?
            float cosi = -dotp(nhit, raydir);
            float k = 1 - eta * eta * (1 - cosi * cosi);
            vect3f refrdir = {raydir.x * eta + nhit.x * (eta *  cosi - sqrt(k)), raydir.y * eta + nhit.y * (eta *  cosi - sqrt(k)), raydir.z * eta + nhit.z * (eta *  cosi - sqrt(k))};
            vect3f_normalize(&refrdir);
            vect3f refr_temp = {phit.x - nhit.x * bias, phit.y - nhit.y * bias, phit.z - nhit.z * bias};
            refraction = trace(refr_temp, refrdir, spheres, depth + 1);
        }
        // the result is a mix of reflection and refraction (if the sphere is transparent)
        ///////////////////////////////////////////////////////////////////////////////////////////////
        surfaceColor.x = (reflection.x * fresneleffect + refraction.x * (1 - fresneleffect) * sphere->transparency) * sphere->surfaceColor.x;
        surfaceColor.y = (reflection.y * fresneleffect + refraction.y * (1 - fresneleffect) * sphere->transparency) * sphere->surfaceColor.y;
        surfaceColor.z = (reflection.z * fresneleffect + refraction.z * (1 - fresneleffect) * sphere->transparency) * sphere->surfaceColor.z;
        ///////////////////////////////////////////////////////////////////////////////////////////////
    }
    else {
        /*
        // it's a diffuse object, no need to raytrace any further
        for (unsigned i = 0; i < spheres.size(); ++i) {
            if (spheres[i].emissionColor.x > 0) {
                // this is a light
                Vec3f transmission = 1;
                Vec3f lightDirection = spheres[i].center - phit;
                lightDirection.normalize();
                for (unsigned j = 0; j < spheres.size(); ++j) {
                    if (i != j) {
                        float t0, t1;
                        if (spheres[j].intersect(phit + nhit * bias, lightDirection, t0, t1)) {
                            transmission = 0;
                            break;
                        }
                    }
                }
                surfaceColor += sphere->surfaceColor * transmission *
                std::max(float(0), nhit.dot(lightDirection)) * spheres[i].emissionColor;
            }
        }
        */
    }
	vect3f ret = {surfaceColor.x + sphere->emissionColor.x, surfaceColor.y + sphere->emissionColor.y, surfaceColor.z + sphere->emissionColor.z};
    return ret;////////////////////////////////////////
}


__global__ void devtracer (int Height, int Width, float angle, float aspectratio, Sphere* spheres, vect3f* pixel){
	int tid = blockIdx.x*blockDim.x + threadIdx.x;
	int y = tid/Width;
	int x = tid%Width;
	
	if (tid < Height*Width) {
		float invWidth = 1 / float(Width), invHeight = 1 / float(Height);
		
		float xx = (2 * ((x + 0.5) * invWidth) - 1) * angle * aspectratio; //
		float yy = (1 - 2 * ((y + 0.5) * invHeight)) * angle; //Centre of the pixel
		vect3f raydir = {xx, yy, -1}; //-1 is where eyes are at
		//raydir.x = xx; raydir.y = yy; raydir.z = -1;
		
		vect3f_normalize(&raydir);
		vect3f trace_temp = {0,0,0};
		pixel[tid] = trace(trace_temp, raydir, spheres, 0); ///origin kept as 0,0
	}
}


void render(const Sphere *spheres)
{
	unsigned width = 640, height = 480;
    vect3f *pixel = (vect3f*) malloc(width * height * sizeof(float));
    vect3f *gpu_pixel;
    Sphere* gpu_spheres;
    
    float fov = 30, aspectratio = width / float(height);
    float angle = tan(M_PI * 0.5 * fov / 180.);  ///tan!!!!!!!
    vect3f *image = pixel;
    	
    cudaMalloc(&gpu_pixel, width*height*sizeof(float));
    cudaMalloc(&gpu_spheres, sizeof(spheres));
    
    int griddim = (width*height)/1024 + 1;
    int num_spheres = sizeof(spheres)/sizeof(spheres[0]);
    Sphere* arr = (Sphere*)malloc(sizeof(Sphere) * num_spheres);
 
    // calling constructor
    // for each index of array
    /*for (int i = 0; i < num_spheres; i++) {
        arr[i] = spheres[i];
    }
    */
    //Sphere copysphere[spheres.size()];
    //std::copy(spheres.begin(),spheres.end(),copysphere);
    
    cudaMemcpy((void*)gpu_spheres, (void*)spheres, sizeof(arr), cudaMemcpyHostToDevice);
	
    devtracer<<<griddim, 1024>>>(height, width, angle, aspectratio, gpu_spheres, gpu_pixel);
    
    cudaMemcpy(pixel, gpu_pixel, width*height*sizeof(float), cudaMemcpyDeviceToHost);
    
	////////////////////////////////////////////////////////FILE NOT SAVED
    // Save result to a PPM image (keep these flags if you compile under Windows)
    std::ofstream ofs("./Ray2.ppm", std::ios::out | std::ios::binary);
    ofs << "P6\n" << width << " " << height << "\n255\n";
    for (unsigned i = 0; i < width * height; ++i) {
        ofs << (unsigned char)(std::min(float(1), image[i].x) * 255) <<
               (unsigned char)(std::min(float(1), image[i].y) * 255) <<
               (unsigned char)(std::min(float(1), image[i].z) * 255);
    }
    ofs.close();
    delete [] image;
    
}


int main(int argc, char **argv)
{   auto start = std::chrono::high_resolution_clock::now();
    srand48(13);
    //std::vector<Sphere> spheres;
    Sphere spheres[6];
    // position, radius, surface color, reflectivity, transparency, emission color
    vect3f vref_center = {0.0, -10004, -20}, vref_sc = {0.20, 0.20, 0.20};
    vect3f v1_center = { 0.0, 0, -20}, v1_sc = {1.00, 0.32, 0.36};
    vect3f v2_center = {5.0, -1, -15}, v2_sc = {0.90, 0.76, 0.46};
    vect3f v3_center = {5.0, 0, -25}, v3_sc = {0.65, 0.77, 0.97};
    vect3f v4_center = {-5.5, 0, -15}, v4_sc = {0.90, 0.90, 0.90};
    vect3f vlight_center = {0.0, 20, -30}, vlight_sc = {0.00, 0.00, 0.00};
    vect3f vtemp = {0.0,0.0,0.0};
    vect3f light_temp = {3, 0, 0};
    /*
    spheres.push_back(Sphere(vref_center, 10000, vref_sc, 0, 0.0, vtemp));
    spheres.push_back(Sphere(v1_center, 4, v1_sc, 1, 0.5, vtemp));
    spheres.push_back(Sphere(v2_center, 2, v2_sc, 1, 0.0, vtemp));
    spheres.push_back(Sphere(v3_center, 3, v3_sc, 1, 0.0, vtemp));
    spheres.push_back(Sphere(v4_center, 3, v4_sc, 1, 0.0, vtemp));
    // light //////////////////////////////////////////////// last param?
    
    spheres.push_back(Sphere(vlight_center, 3, vlight_sc, 0, 0.0, light_temp)); 
    */
    spheres[0] = {vref_center, 10000, (10000*10000), vref_sc, 0, 0.0, vtemp};
    spheres[1] = {v1_center, 4, (4*4), v1_sc, 1, 0.5, vtemp};
    spheres[2] = {v2_center, 2, (2*2), v2_sc, 1, 0.0, vtemp};
    spheres[3] = {v3_center, 3, (3*3), v3_sc, 1, 0.0, vtemp};
    spheres[4] = {v4_center, 3, (3*3), v4_sc, 1, 0.0, vtemp};
    spheres[5] = {vlight_center, 3, (3*3), vlight_sc, 0, 0.0, light_temp};
    render(spheres);
    auto stop = std::chrono::high_resolution_clock::now();
    auto duration = std::chrono::duration_cast<std::chrono::microseconds>(stop - start);
    std::cout << duration.count() << std::endl;
    
    return 0;
}
