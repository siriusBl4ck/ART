package rt;

import FloatingPoint::*;
import RegFile::*;

//params
typedef 1024 DMEM_SIZE;

//consts
typedef 3.141592653589793 M_PI;
typedef 100000000 INFI;

typedef struct {Float x; Float y; Float z;} Vec3f;
typedef struct {Vec3f center; Float radius; Float radius2; Vec3f surfaceColor; Vec3f emissionColor; Float transparency; Float reflection;} Sphere;


// computes the ray intersection for a given ray
interface trace_ifc;
    method Action putRayAndStart(Vec3f orig, Vec3f dir);
    method ActionValue #(Vec3f) getRGB();
endinterface

module mkTrace(trace_ifc);
    //3d data memory
    RegFile#(Bit#(5), Bit#(11)) dmem <- mkRegFileLoad ("3d_data.mem");
    /*
    Reg#(Maybe#(Sphere)) dmem[DMEM_SIZE];
    for (Integer j = 0; j < DMEM_SIZE; j = j + 1) dmem[j] <- mkReg(tagged Invalid);
    */
    //ray
    Reg#(Vec3f) rayorig <- mkReg(0);
    Reg#(Vec3f) raydir <- mkReg(0);
    Reg#(Vec3f) rgb <- mkReg(0);

    //flags
    Reg#(Bool) rdy <- mkReg(True);
    Reg#(Bool) init <- mkReg(False);

    //intersection
    Reg#(Float) tnear <- mkReg(0);
    reg#(Bool) intersect <- mkReg(1);

    //sphere counter
    Reg#(int) cnt <- mkReg(0);
    Reg#(int) totSpheres <- mkReg(0);

    rule compute(init && !rdy && (cnt < totSpheres))
        //main ray intersection code goes here

        //get sphere
        Sphere currSphere = dmem[cnt];

        // check if ray intersects the sphere
        Vec3f l;
        l.x = currSphere.center.x - rayorig.x;
        l.y = currSphere.center.y - rayorig.y;
        l.z = currSphere.center.z - rayorig.z;
        
        Float tca = (l.x * raydir.x) + (l.y * raydir.y) + (l.z * raydir.z);
        
        //ray does not intersect the sphere (ray points away from the direction from ray origin to center)
        if (tca < 0) intersect <= 0;

        Float d2 = (l.x * l.x) + (l.y * l.y) + (l.z * l.z) - (tca * tca);

        // the distance of ray from center is larger than the radius. no intersections
        if (d2 > currSphere.radius2) intersect <= 0;

        Float thc = sqrtFP((currSphere.radius2 - d2), Rnd_Zero);

        // t0 and t1 are the two distances along raydir at which intersections happen
        Float t0 = tca - thc;
        Float t1 = tca + thc;

        cnt <= cnt + 1;
        rdy <= True;
    endrule

    method Action putRayAndStart(Vec3f orig, Vec3f dir);
        rdy <= False;
        rayorig <= orig;
        raydir <= dir;
    endmethod

endmodule: trace


/*
// Memory unit for 3d data
interface modelDMem_ifc;
    method Sphere getData(int addr);
    method Action putData(int addr, Sphere sp);
endinterface

module mkModelDMem(modelDMem_ifc);
    

    method Action putData(int addr, Sphere sp);
        if (addr < DMEM_SIZE && addr >= 0) dmem[addr] <= sp;
    endmethod

    method Sphere getData(int addr);
        if (addr < DMEM_SIZE && addr >= 0) return dmem[addr];
        else return dmem[0];
        // TODO: change this else condition to something more appropriate
    endmethod
endmodule: modelDMem
*/
endpackage:rt
