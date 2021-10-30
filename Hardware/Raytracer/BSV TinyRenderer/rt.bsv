package rt;

import FloatingPoint::*;

//params
typedef 1024 DMEM_SIZE;

//consts
typedef 3.141592653589793 M_PI;
typedef 100000000 INFI;

// Type definitions
typedef FloatingPoint#(11,52) FDouble;
typedef FloatingPoint#(8,23)  FSingle;

typedef struct {FSingle x; FSingle y; FSingle z;} Vec3f;
typedef struct {Vec3f center; FSingle radius; FSingle radius2; Vec3f surfaceColor; Vec3f emissionColor; FSingle transparency; FSingle reflection;} Sphere;


// computes the ray intersection for a given ray
interface trace_ifc;
    method Action putRayAndStart(Vec3f orig, Vec3f dir);
    method ActionValue #(Vec3f) getRGB();
    method Action memRead(String filename);
endinterface

module mkTrace(trace_ifc);
    //3d data memory
    Reg#(Maybe#(Sphere)) dmem[DMEM_SIZE];
    for (Integer j = 0; j < DMEM_SIZE; j = j + 1) dmem[j] <- mkReg(tagged Invalid);
    
    //ray
    Reg#(Vec3f) rayorig <- mkReg(0);
    Reg#(Vec3f) raydir <- mkReg(0);
    Reg#(Vec3f) rgb <- mkReg(0);

    //flags
    Reg#(Bool) rdy <- mkReg(True);
    Reg#(Bool) init <- mkReg(False);

    //intersection
    Reg#(FSingle) tnear <- mkReg(0);

    //sphere counter
    Reg#(int) cnt <- mkReg(0);
    Reg#(int) totSpheres <- mkReg(0);

    rule compute(init && !rdy && (cnt < totSpheres))
        //main ray intersection code goes here
        
        //get sphere
        Sphere currSphere = dmem[cnt];

        cnt <= cnt + 1;
        rdy <= True;
    endrule

    method Action putRayAndStart(Vec3f orig, Vec3f dir);
        rdy <= False;
        rayorig <= orig;
        raydir <= dir;
    endmethod

    method Action memRead(String filename);
        //TODO: File reading code goes here
        init <= True;
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
