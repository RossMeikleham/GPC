using namespace std;
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <iostream>
#include <sys/time.h>

namespace GPRM {

namespace Kernel {

class Fib {
	public:
		int fib(int);
		int timer(int nothing);
		int fib_iter(int);
        int start_timer();
};

}
}
