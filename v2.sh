#!/bin/bash

set -e
set -v

#======== dlopen/dlsym is used
g++ -g -O0 -fpermissive -o test ./ast.cpp -Wl,-ldl

./test 0 0
./test 129 129
./test -129 -129
./test 0x0 0x0
./test 0x0a 0x0a
./test 0xffffffff 0xffffffff
./test 0xa0 0xa0
./test "  0xa0" 0xa0

./test -+129 -129
./test -+-++-129 -129
./test ~0x5555AAAA 0xFFFFFFFFAAAA5555
./test ~~0x5555AAAA 0x5555AAAA


#======= test for snum =======
./test -0xa0 -0xa0
./test !0xa0 0
./test !0x0 1
./test ~0x0 0xFFFFFFFFFFFFFFFF
./test ~0x1 0xFFFFFFFFFFFFFFFE




#======= test for t1 =======
./test 1*2 $((1*2))
./test 1123*21/2*0xA123 $((1123*21/2*0xA123))
./test 1123*21/2*0xA123%512324 $((1123*21/2*0xA123%512324))

#======= test for t2 =======
./test 1+2 $((1+2))
./test 1+2*3 $((1+2*3))
./test 1+2*3-4*5 $((1+2*3-4*5))


#======= test for bin ops  =======
./test "7>=8" $((7>=8))
./test "7<8" $((7<8))
./test "7 <= 7" $((7<=7))

./test "0x55 & 123 | 0xBACD" $((0x55 & 123 | 0xBACD))
./test "7 < 7 && 8-2>=6" $((7 < 7 && 8-2>=6))

./test "1+2*3>4*5" $((1+2*3>4*5))
./test "1+2*3<4*5" $((1+2*3<4*5))
./test "1+2*(3+4)*5" $((1+2*(3+4)*5))
./test "1+2*(3+(4+6)*5)*5" $((1+2*(3+(4+6)*5)*5))

./test "0x12345678 >> 1+2*3" $((0x12345678 >> 1+2*3))
./test "1+2*3 << 1+2*3" $((1+2*3 << 1+2*3))

./test "1>2?10:20" 20
./test "1<2?10:20" 10

./test "1>2?10+2*3:20-1*2" 18
./test "1<2?10+2*3:20-1*2" 16

#========= test assign ================
./test "int a;a=3" 3
./test "int a,b;a=3,b=2,b" 2
./test "int a,b,c;a=3,b=c=a+=2" 5
./test "int a,b,c;a=3,b=c-=a+=2" -5
./test "int a,b,c;a=3,b-=c-=a+=2" 5

./test "int a;a=2,a+2*3" 8
./test "int a,b;a=2,b=30,80*(a<b?(a+2*3):120)" 640
./test "int a,b;a=2,b=30,80*(a>b?(a+2*3):12)" 960

#========= test pointer ================
./test "int a,b;a=0,b=&a,*b=123,a" 123

#========= test early terminate
./test "int a,b;a=2,b=30,a>b&&(a=0), a" 2
./test "int a,b;a=2,b=30,a<b&&(a=0), a" 0
./test "int a,b;a=2,b=30,a>b||(a=0), a" 0
./test "int a,b;a=2,b=30,a<b||(a=0), a" 2

./test "int a,b;a=2,b=30,a>b?(a=0):(b=0), b" 0
./test "int a,b;a=2,b=30,a>b?(a=0):(b=0), a" 2

./test "int a,b;a=2,b=30,a<b?(a=0):(b=0), b" 30
./test "int a,b;a=2,b=30,a<b?(a=0):(b=0), a" 0

#========= test if
./test "int a,b,c;a=1,b=2,c=0;if(a<b) c=4; else c=2; c" 4
./test "int a,b,c;a=1,b=2,c=0;if(a<b) {a=2;c=a*b;} else c=2; c" 4

./test "int a,b,c;a=1,b=2,c=0;if(a>b) c=4; else c=2; c" 2
./test "int a,b,c;a=1,b=2,c=0;if(a>b) {a=2;c=a*b;} else {a=3;c=a+b;} c" 5

#========= test while
./test "int a,b,c;a=1,b=2,c=0; while(a<10) a+=1; a" 10
./test "int a,b;a=17000; while(a>=1){ b=2; while(b<a) { if(a%b==0) break; b+=1;}; if(b==a) break; a-=1;} a" 16993


#========= test print
./test 'int a,b,c;a=1; c=0; print("probe primes:"); while(a<100){ b=2; while(b<a) { if(a%b==0) break; b+=1;}; if(b==a) {c+=1; print(a,",");}; a+=1;} print("\n"); c;' 25

#========= test function call
./test 'int a,b,c; int sum(int a,int b){c=2;return a+b;} b=sum(2,3);' 5

./test 'int a,b,c; int is_prime(int a){int i;i=2; while (i<a) {if(a%i == 0) return 0; i+=1;} return 1;}  while(a<100){a+=1; if(is_prime(a)) print(a,",");} '
