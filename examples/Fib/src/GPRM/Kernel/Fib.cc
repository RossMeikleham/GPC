#include "Fib.h"
#define RESET_COLOR "\e[m"
#define MAKE_BLUE "\e[34m"

timeval t;
long start;

int GPRM::Kernel::Fib::fib(int n) {
	int x,y;
	 if (n<2) return n;
	 x = fib(n-1);
	 y = fib(n-2);
	 return x+y;
 }

 int GPRM::Kernel::Fib::fib_iter(int n) {
	if (n < 2) return n;
    	int prevPrev = 0;
   	int prev = 1;
    	int result = 0;

    	for (int i = 2; i <= n; i++)
    	{
        	result = prev + prevPrev;
        	prevPrev = prev;
        	prev = result;
    	}
    	return result;
}
 int GPRM::Kernel::Fib::start_timer() {
    gettimeofday(&t, NULL);
    start = (t.tv_sec * 1000) + (t.tv_usec / 1000);
 
 }

 int GPRM::Kernel::Fib::timer(int res)
 {
    gettimeofday(&t, NULL);
 	long end = (t.tv_sec * 1000) + (t.tv_usec / 1000);
 	printf("Timer: %d\n",end-start);
    printf("Result: %d\n", res);
 	return 0;
 }

