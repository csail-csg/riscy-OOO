#include <math.h>
// #include <fenv.h>
// #pragma STDC FENV_ACCESS ON
// // store the original rounding mode
// const int originalRounding = fegetround( );
// // establish the desired rounding mode
// fesetround(FE_TOWARDZERO);
// //FE_TONEAREST
// //FE_UPWARD
// //FE_DOWNWARD
// //FE_TOWARDZERO
// // do whatever you need to do ...
// 
// // ... and restore the original mode afterwards
// fesetround(originalRounding);

    typedef union {
        unsigned int ui;
        float f;
    } ui_f;
    typedef union {
        unsigned long long ull;
        double d;
    } ull_d;

    unsigned int sqrt_float_c( unsigned int in1, unsigned char rmode ) {
        // ignore rmode for now
        ui_f in, out;
        in.ui = in1;
        out.f = sqrtf(in.f);
        return out.ui;
    }
    unsigned long long sqrt_double_c( unsigned long long in1, unsigned char rmode ) {
        // ignore rmode for now
        ull_d in, out;
        in.ull = in1;
        out.d = sqrt(in.d);
        return out.ull;
    }
