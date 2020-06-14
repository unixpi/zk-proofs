template IsZero() {
    signal input in;
    signal output out;

    signal inv;

    inv <-- in!=0 ? 1/in : 0;

    out <== -in*inv +1;
    in*out === 0;
}


template IsEqual() {
    signal input in[2];
    signal output out;

    component isz = IsZero();

    in[1] - in[0] ==> isz.in;

    isz.out ==> out;
}


template PlusOrMinusOne() {
	 signal input x;
	 signal output out;


	 out <-- (x-1)*(x+1);
	 out === 0;
}

component main = PlusOrMinusOne();

/*
template CheckPlusOrMinusOne(n) {
	 signal private input m[n];

         component intermediates[n];
	 signal output out;
}
*/


