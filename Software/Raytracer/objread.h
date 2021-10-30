#ifndef OBJREAD_H
#define OBJREAD_H

#include <iostream>
#include <fstream>
#include <unordered_map>
#include <vector>
#include <sstream>
#include <cmath>
using namespace std;

template <typename T>
class Vec3
{
public:
    T x, y, z;
    Vec3() : x(T(0)), y(T(0)), z(T(0)) {}
    Vec3(T xx) : x(xx), y(xx), z(xx) {}
    Vec3(T xx, T yy, T zz) : x(xx), y(yy), z(zz) {}
    Vec3 &normalize()
    {
        T nor2 = length2();
        if (nor2 > 0)
        {
            T invNor = 1 / sqrt(nor2);
            x *= invNor, y *= invNor, z *= invNor;
        }
        return *this;
    }
    Vec3<T> operator*(const T &f) const { return Vec3<T>(x * f, y * f, z * f); }
    Vec3<T> operator*(const Vec3<T> &v) const
    {
        return Vec3<T>(x * v.x, y * v.y, z * v.z);
    }
    T dot(const Vec3<T> &v) const { return x * v.x + y * v.y + z * v.z; }
    Vec3<T> cross(const Vec3<T> &v) const
    {
        return Vec3<T>(y * v.z - v.y * z, z * v.x - v.z * x, x * v.y - v.x * y);
    }
    Vec3<T> operator-(const Vec3<T> &v) const
    {
        return Vec3<T>(x - v.x, y - v.y, z - v.z);
    }
    Vec3<T> operator+(const Vec3<T> &v) const
    {
        return Vec3<T>(x + v.x, y + v.y, z + v.z);
    }
    Vec3<T> &operator+=(const Vec3<T> &v)
    {
        x += v.x, y += v.y, z += v.z;
        return *this;
    }
    Vec3<T> &operator*=(const Vec3<T> &v)
    {
        x *= v.x, y *= v.y, z *= v.z;
        return *this;
    }
    Vec3<T> operator-() const { return Vec3<T>(-x, -y, -z); }
    T length2() const { return x * x + y * y + z * z; }
    T length() const { return sqrt(length2()); }
    friend std::ostream &operator<<(std::ostream &os, const Vec3<T> &v)
    {
        os << "[" << v.x << " " << v.y << " " << v.z << "]";
        return os;
    }
};

typedef Vec3<float> Vec3f;

class Triangle
{
public:
    Vec3f vertex1, vertex2, vertex3;   /// locations of vertices
    Vec3f surfaceColor, emissionColor; /// surface color and emission (light)
    float transparency, reflection;    /// surface transparency and reflectivity
    Vec3f normal;
    Triangle(const Vec3f &vertex1, const Vec3f &vertex2, const Vec3f &vertex3,
             const Vec3f &sc, const float &refl = 0, const float &transp = 0,
             const Vec3f &ec = 0)
        : vertex1(vertex1), vertex2(vertex2), vertex3(vertex3), surfaceColor(sc),
          emissionColor(ec), transparency(transp), reflection(refl)
    {
        Vec3f E1 = vertex3 - vertex1;
        Vec3f E2 = vertex2 - vertex1;
        normal = E2.cross(E1);
        normal.normalize();
    }
    //[comment]
    // Compute a ray-triangle intersection using the geometric solution
    // Logic at
    // https://stackoverflow.com/questions/42740765/intersection-between-line-and-triangle-in-3d
    bool intersect(const Vec3f &rayorig, const Vec3f &raydir, float &t0) const
    {
        Vec3f E1 = vertex2 - vertex1;
        Vec3f E2 = vertex3 - vertex1;
        Vec3f N = E1.cross(E2);
        float det = -N.dot(raydir);
        float invdet = 1.0 / det;
        Vec3f AO = rayorig - vertex1;
        Vec3f DAO = AO.cross(raydir);
        float u = E2.dot(DAO) * invdet;
        float v = -E1.dot(DAO) * invdet;
        t0 = AO.dot(N) * invdet;
        if ((t0 >= 0.0) && (u >= 0.0) && (v >= 0.0) && (u + v <= 1.0))
        {
            return true;
        }
        return false;
    }
    void cout() {
        std::cout<<"vertex 1:"<<vertex1<<endl;
        std::cout<<"vertex 2:"<<vertex2<<endl;
        std::cout<<"vertex 3:"<<vertex3<<endl;
    }
};

class Sphere
{
public:
    Vec3f center;                      /// position of the sphere
    float radius, radius2;             /// sphere radius and radius^2
    Vec3f surfaceColor, emissionColor; /// surface color and emission (light)
    float transparency, reflection;    /// surface transparency and reflectivity
    Sphere(const Vec3f &c, const float &r, const Vec3f &sc, const float &refl = 0,
           const float &transp = 0, const Vec3f &ec = 0)
        : center(c), radius(r), radius2(r * r), surfaceColor(sc),
          emissionColor(ec), transparency(transp), reflection(refl)
    { /* empty */
    }
    //[comment]
    // Compute a ray-sphere intersection using the geometric solution
    //[/comment]
    bool intersect(const Vec3f &rayorig, const Vec3f &raydir, float &t0,
                   float &t1) const
    {
        Vec3f l = center - rayorig;
        float tca = l.dot(raydir);
        if (tca < 0)
            return false;
        float d2 = l.dot(l) - tca * tca;
        if (d2 > radius2)
            return false;
        float thc = sqrt(radius2 - d2);
        t0 = tca - thc;
        t1 = tca + thc;

        return true;
    }
};

vector<Triangle> parser(string filename);

#endif