/*
Circuit to help prove you know a solution to an instance of the partition problem
Inputs:
1. public array which holds the values in the set, say for example [4,11,8,1]
2. private array which shows which subset is associated with the value in question (the satisfying assignment)

for example, the satisfying assignment in this case would be [1,-1,1,-1]
(4 and 8 in one subset,  11 and 1 in the other)

The circuit does the following:
1. constrain the  dot product of the two arrays to be zero
2. constrain all values in the satisfying assignment to be either 1 or -1
*/

template Multiplier() {
	 signal private input a;
	 signal private input b;
	 signal output c;

	 c <== a*b;
}

template DotProduct(n) {
	 signal input l[n];
	 signal private input m[n];

	 component intermediates[n];
	 signal output out;
	 var sum = 0

	 for(var i=0; i<n; i++) {
	 	 (m[i]-1) * (m[i]+1) === 0;
	 	 intermediates[i] = Multiplier();
		 intermediates[i].a <== l[i];
		 intermediates[i].b <== m[i];
	 	 sum = sum + intermediates[i].c
	 }
	 out <-- sum;
	 out === 0;
}


component main = DotProduct(4);