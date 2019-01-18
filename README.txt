Haoning Hu
CSC 454
Assignment 8
12/2/2018

For this project I use ocaml to implement the compiler. To run my compiler, enter command "ocaml compiler.ml test_flie.c" where "test_file.c" is the name of the file you want to test. The result will be printed out. As far as I know, my compiler can successfully generate the code for all the test cases, but only ab.c, loop_while.c and Meaningoflife.c can be compiled by "clang -o" and get corresponding result.(cannot be compiled by "gcc -o" because of error "variably modified 'mem' at file scope int mem[N];", but I think that is okay) The file automaton.c cannot be compiled by my compiler because it predefine functions and rewrite them, my compiler cannot handle situation like that it will get confused. Sample code from the slides shows that jumptable should be defined after each callee function and only in this way it can be compiled by "clang -o" but what if there are more than one callee function, wouldn't there be a lot of jumptable? Don't know how to make it work. The file mandel.c cannot be compiled by my compiler because its functions take more than two parameters, it requires a lot of work to change my compiler to support that, don't get enough time to make it work either.

In assignment 8, compiler is basically another translator, their differences are compiler does not have things like "local[1],global[1]", all the variables will be stored in array mem[], which is lke the stack of this program. When you want to call another function, you need pre-jump code and post-jump code, to push parameters of the callee function into the stack and change the register "base" and "top". I use the name "base" and "top" instead of "sp" and "fp". Each function will have prologue and epilogue code to initialize or reset "base" and "top" Also there will be no functions except one huge main function, this function include all functions in the source code. All the functions from the source code become labels like "mainFunc:" and "addFunc:" when you want to call any function just use "goto func;" Go to another function is easy, but come back is not. We use this jumpTable, it is a switch statement, where it will go depends on the value of "jumpReg". I notice that prologue/epilogue and pre-jump/post-jump have some standard format, for example caller pre-jump code always looks like this:

For parameters from 1 to k
mem[top+i-1] = actual parameter i
top = top + k
mem[top] = base
mem[top+1] = return_label
top = top + 3  
jump to callee_entry 

Callee function always looks like this:

Callee prologue:
base = top
top = base + local_array_size
Callee epilogue:
store return value to mem[base-1]
top = base
return_address_label = mem[base-2] • jump to return_address_label
Caller re-entry:
copy return value from mem[top-1] • base = mem[top - 3]
top = base + local_array_size

Jumptable looks like this:

jumpTable:
 switch(jumpReg) {
case 1:
   goto label_1; break;
 default: assert(0);}

This actually makes it much easier. I use the parser and translator written by out group in A7, it uses recursive descent and translate the code while parsing, when it gets some tokens need to be changed it will just put the translated tokens into a list, and in the end we print this list to get the translated code. For the compiler I just use my translator to translate the code into something else, when a function is called just add the pre-jump code into the list, when the calling is over, just add post-jump code to the list. Also, I add some variables and lists into the translator to implement certain functions, for example I use some Boolean variables to see if there is function calling when the parser/translator gets into another function. But there are still many problems. For example, all the function use the same path to parse and translate, but they should have different output in A8, it takes me a lot of time try to implement this, because of the limitation of our method, I still don't know how to handle some problems. At the start of each program it will have some predefined stuff like this:
#define top mem[0]
#define base mem[1]
#define jumpReg mem[2]
#define membase 3
int const N=2000;
int mem[N]; 
Where "base" and "top" are the registers "fp" and "sp", "jumpReg" is another register used to jump back to jumplabel.

All in all, finishing this assignment is a great experience, hope I can get a good grade. 
