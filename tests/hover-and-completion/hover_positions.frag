#version 430 core

int       value  = 0;
const int value_ = 1;

void foo() {
    // Past-the-end hovers.
    // Should only trigger when cursor is adjacent to the token.
    value;

    value ;
    value
;

    value/* w */;
    value//
;

    value;value_;

}


