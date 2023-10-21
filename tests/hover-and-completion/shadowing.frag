#version 430 core


int       val;
const int cval = 1;

void main() {
    val;  // int
    cval; // const int

    {
        {
            const int val = cval; // const int = const int
        }
        int cval = cval; // int = const int
        {
            cval; // int
            val;  // int
        }
    }
    cval; // const int
    val;  // int


    int       cval = val;  // int = int
    const int val  = cval; // const int = int

    val;  // const int
    cval; // int

}


void foo(const int val, int cval) {
    val;  // const int
    cval; // int
}
