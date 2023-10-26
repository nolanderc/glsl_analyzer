#version 430 core


struct AAA {
    int   f1;
    float f2;
    uint  fa; // Exclusive to AAA
};

struct BBB {
    float f1;
    int   f2;
    bool  fb; // Exclusive to BBB
};

struct CCC {
    AAA aaa; // To maybe confuse with globals
    BBB bbb;
    AAA  f1;
    BBB  f2;
};


const AAA aaa = AAA(1, 1.0, 1);
const BBB bbb = BBB(1.0, 1, true);
const CCC ccc = CCC(aaa, bbb, aaa, bbb);


void foo() {
    aaa; // const AAA
    bbb; // const BBB
    ccc; // const CCC

    // Single subfield expansion
    aaa.f1; // int
    bbb.f1; // float
    ccc.f1; // AAA

    // Nested subfield expansion
    ccc.aaa.f1; // int
    ccc.aaa.f2; // float
    ccc.aaa.fa; // uint
    ccc.bbb.f1; // float
    ccc.bbb.f2; // int
    ccc.bbb.fb; // bool
}

