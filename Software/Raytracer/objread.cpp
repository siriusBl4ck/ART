#include <iostream>
#include <fstream>
#include <unordered_map>
#include <vector>
#include <sstream>
#include <cmath>
#include "objread.h"
using namespace std;

vector<Triangle> parser(string filename) {
    fstream fio;
    string line;
    fio.open(filename,ios::in);
    vector<Triangle> triangles;
    unordered_map<int,Vec3f> vertexMap;
    int vertexCount;
    while(getline(fio,line)) {
        cout<<line<<endl;
        std::stringstream eachLine(line);
        string word;
        eachLine>>word;
        if(word=="o") {
            vertexMap.clear();
            vertexCount=1;
            eachLine>>word;
            cout<<word;
        }
        else if(word=="v"){
            eachLine>>word;
            double x = stof(word);
            eachLine>>word;
            double z = stof(word);
            z-=10;
            eachLine>>word;
            double y = stof(word);
            vertexMap[vertexCount++]=Vec3f(x,y,z);
        }
        else if(word=="f"){
            eachLine>>word;
            std::stringstream firstWord(word);
            string temp;
            getline(firstWord, temp, '/');
            Vec3f vertex1=vertexMap[stoi(temp)];
            eachLine>>word;
            std::stringstream secondWord(word);
            getline(secondWord, temp, '/');
            Vec3f vertex2=vertexMap[stoi(temp)];
            eachLine>>word;
            std::stringstream thirdWord(word);
            getline(thirdWord, temp, '/');
            Vec3f vertex3=vertexMap[stoi(temp)];
            if(eachLine>>word){
                std::stringstream fourthWord(word);
                getline(fourthWord,temp,'/');
                Vec3f vertex4=vertexMap[stoi(temp)];
                triangles.push_back(Triangle(vertex1,vertex4,vertex3,Vec3f(0.2,0.2,0.2),1,0.5));
            }
            triangles.push_back(Triangle(vertex1,vertex2,vertex3,Vec3f(0.2,0.2,0.2),1,0.5));
        }
    }
    fio.close();
    return triangles;
}
