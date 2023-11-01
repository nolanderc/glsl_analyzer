#version 430 core


int iarr[3] = { 1, 2, 3 };
int iarr_implicit[];
int iarr_nested[2][3];
const int ciarr[3] = { 1, 2, 3 };
const int ciarr_nested_implicit[][3] = { { 1, 2, 3 }, { 4, 5, 6 } };

struct Elem {
    int field1;
};

const Elem elements[] = { {1}, {2}, {3} };

void main() {

    ciarr;
    const int ciarr[] = { 1, 2, 3 };
    ciarr;

    iarr_implicit;

    iarr_nested[0] = ciarr;

    ciarr_nested_implicit;

}

void foo(const Elem elements[3]) {

    elements;

    // Arrays have no fields.
    //       v
    elements. ;

    elements[0].field1;

}
