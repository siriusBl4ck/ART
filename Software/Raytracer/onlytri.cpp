// [header]
// A very basic raytracer example.
// [/header]
// [compile]
// c++ -o raytracer -O3 -Wall raytracer.cpp
// [/compile]
// [ignore]
// Copyright (C) 2012  www.scratchapixel.com
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// [/ignore]
#include <cassert>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <chrono>
#include <vector>
#include "objread.h"

#if defined __linux__ || defined __APPLE__
// "Compiled for Linux
#else
// Windows doesn't define these values by default, Linux does
#define M_PI 3.141592653589793
#define INFINITY 1e8
#endif


//[comment]
// This variable controls the maximum recursion depth
//[/comment]
#define MAX_RAY_DEPTH 5

float mix(const float &a, const float &b, const float &mix)
{
    return b * mix + a * (1 - mix);
}

//[comment]
// This is the main trace function. It takes a ray as argument (defined by its
// origin and direction). We test if this ray intersects any of the geometry in
// the scene. If the ray intersects an object, we compute the intersection
// point, the normal at the intersection point, and shade this point using this
// information. Shading depends on the surface property (is it transparent,
// reflective, diffuse). The function returns a color for the ray. If the ray
// intersects an object that is the color of the object at the intersection
// point, otherwise it returns the background color.
//[/comment]
Vec3f trace(const Vec3f &rayorig, const Vec3f &raydir,
            const std::vector<Triangle> &triangles, const std::vector<Sphere> &spheres,const int &depth)
{
    // if (raydir.length() != 1) std::cerr << "Error " << raydir << std::endl;
    float tnear = INFINITY;
    const Triangle *triangle = NULL;
    // find intersection of this ray with the sphere in the scene
    for (unsigned i = 0; i < triangles.size(); ++i)
    {
        float t0 = INFINITY;
        if (triangles[i].intersect(rayorig, raydir, t0))
        {
            if (t0 < tnear)
            {
                tnear = t0;
                triangle = &triangles[i];
            }
        }
    }
    // if there's no intersection return black or background color
    if (tnear == INFINITY) return Vec3f(2);
    Vec3f surfaceColor = 0;                                 // color of the ray/surfaceof the object intersected by the ray
    Vec3f phit = rayorig + raydir * tnear; // point of intersection
    Vec3f nhit = triangle->normal;
    float bias =
        1e-4; // add some bias to the point from which we will be tracing
    bool inside = false;
    if (raydir.dot(nhit) > 0)
        nhit = -nhit, inside = true;
    if ((triangle->transparency > 0 || triangle->reflection > 0) &&
        depth < MAX_RAY_DEPTH)
    {
        float facingratio = -raydir.dot(nhit);
        float fresneleffect = mix(pow(1 - facingratio, 3), 1, 0.1);
        // compute reflection direction (not need to normalize because all vectors
        // are already normalized)
        Vec3f refldir = raydir - nhit * 2 * raydir.dot(nhit);
        refldir.normalize();
        Vec3f reflection =
            trace(phit + nhit * bias, refldir, triangles, spheres,depth + 1);
        Vec3f refraction = 0;
        // if the sphere is also transparent compute refraction ray (transmission)
        if (triangle->transparency)
        {
            float ior = 1.1,
                  eta = (inside) ? ior
                                 : 1 / ior; // are we inside or outside the surface?
            float cosi = -nhit.dot(raydir);
            float k = 1 - eta * eta * (1 - cosi * cosi);
            Vec3f refrdir = raydir * eta + nhit * (eta * cosi - sqrt(k));
            refrdir.normalize();
            refraction =
                trace(phit - nhit * bias, refrdir, triangles, spheres,depth + 1);
        }
        surfaceColor = (reflection * fresneleffect +
                        refraction * (1 - fresneleffect) * triangle->transparency) *
                       triangle->surfaceColor;
    }
    else
    {
        // it's a diffuse object, no need to raytrace any further
        for (unsigned i = 0; i < spheres.size(); ++i)
        {
            if (spheres[i].emissionColor.x > 0)
            {
                // this is a light
                Vec3f transmission = 1;
                Vec3f lightDirection = spheres[i].center - phit;
                lightDirection.normalize();
                for (unsigned j = 0; j <triangles.size(); ++j)
                {
                        float t0;
                        if (triangles[j].intersect(phit + nhit * bias, lightDirection, t0))
                        {
                            transmission = 0;
                            break;
                        }
                }
                surfaceColor += triangle->surfaceColor * transmission *
                                std::max(float(0), nhit.dot(lightDirection)) *
                                spheres[i].emissionColor;
            }
        }
    }
    return surfaceColor + triangle->emissionColor;
}

//[comment]
// Main rendering function. We compute a camera ray for each pixel of the image
// trace it and return a color. If the ray hits a sphere, we return the color of
// the sphere at the intersection point, else we return the background color.
//[/comment]
void render(const std::vector<Triangle> &triangles,const std::vector<Sphere> &spheres)
{
    unsigned width = 640, height = 480;
    Vec3f *image = new Vec3f[width * height], *pixel = image;
    float invWidth = 1 / float(width), invHeight = 1 / float(height);
    float fov = 30, aspectratio = width / float(height);
    float angle = tan(M_PI * 0.5 * fov / 180.);
    // Trace rays
    for (unsigned y = 0; y < height; ++y)
    {
        for (unsigned x = 0; x < width; ++x, ++pixel)
        {   cout<<"y "<<y<<" x "<<x<<endl;
            float xx = (2 * ((x + 0.5) * invWidth) - 1) * angle * aspectratio;
            float yy = (1 - 2 * ((y + 0.5) * invHeight)) * angle;
            Vec3f raydir(xx, yy, -1);
            raydir.normalize();
            *pixel = trace(Vec3f(0), raydir, triangles, spheres,0);
        }
    }
    // Save result to a PPM image (keep these flags if you compile under Windows)
    std::ofstream ofs("./untitled.ppm", std::ios::out | std::ios::binary);
    ofs << "P6\n"
        << width << " " << height << "\n255\n";
    for (unsigned i = 0; i < width * height; ++i)
    {
        ofs << (unsigned char)(std::min(float(1), image[i].x) * 255)
            << (unsigned char)(std::min(float(1), image[i].y) * 255)
            << (unsigned char)(std::min(float(1), image[i].z) * 255);
    }
    ofs.close();
    delete[] image;
}

//[comment]
// In the main function, we will create the scene which is composed of 5 spheres
// and 1 light (which is also a sphere). Then, once the scene description is
// complete we render that scene, by calling the render() function.
//[/comment]
int main(int argc, char **argv)
{   
    auto start = std::chrono::high_resolution_clock::now();
    srand48(13);
    std::vector<Sphere> spheres;
    std::vector<Triangle> triangles=parser(argv[1]);
    for(auto triangle:triangles) {
        triangle.cout();
    }
    // v1,v2,v3,  surface color, reflectivity, transparency, emission color
    // light
    spheres.push_back(Sphere(Vec3f( 0.0,     20, -30),     3, Vec3f(0.00, 0.00, 0.00), 0, 0.0, Vec3f(3))); 
    render(triangles,spheres);
    cout<<"Number of triangles rendered: "<<triangles.size()<<endl;
    auto stop = std::chrono::high_resolution_clock::now();
    auto duration = std::chrono::duration_cast<std::chrono::microseconds>(stop - start);
    cout<<"Run time of program: "<<duration.count()<<endl;
    return 0;
}