#version 430 core

int fun(inout int, out vec4, in float, const float);


int fun_redeclared(int);
int fun_redeclared(int);


// Yeah, this breaks it if you do simple string comparison.
// Same if you add const.
int fun_redeclared_but_with_implicit_in(in int);
int fun_redeclared_but_with_implicit_in(int);


int  fun_overloaded(int);
uint fun_overloaded(uint);

// Another overload declared after main.
// Should not be visible in main.
int fun_overloaded_but_later(int);




void main() {
    int  inout_int;
    vec4 out_vec4;
    // <int (inout int, out vec4, in float, const float)>
    fun(inout_int, out_vec4, 1.0, 1.0);

    // <int (int)>
    fun_redeclared(1);

    // <int (in int)> OR <int (int)>, NOT BOTH
    fun_redeclared_but_with_implicit_in(1);

    // <int (int)> AND <uint (uint)>
    fun_overloaded(1);

    // <int (int)> (NO <uint (uint)>)
    fun_overloaded_but_later(1);

}




int  fun_overloaded(int) { return 0; }
uint fun_overloaded(uint) { return 0; }


int  fun_redeclared(int) { return 0; }


int  fun_redeclared_but_with_implicit_in(int) { return 0; }


int  fun_overloaded_but_later(int) { return 0; }
// This overload was not declared before main()
uint fun_overloaded_but_later(uint) { return 0; }
