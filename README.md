# 449Haskell
Assignment #2: Scheduling Algorithm in Haskall for CPSC 449

CPSC 449 - Assignment problem
The student teams will implement 3 systems (one in Java, one in Haskell, and one in PROLOG) that will all solve the following simple scheduling problem:

We have 8 machines 1,2,3,4,5,6,7,8 and 8 tasks A,B,C,D,E,F,G,H. We need to assign to each machine one of the tasks so that each of the 8 tasks gets its own machine. There are additional so-called hard constraints (see below) that need to be fulfilled by an assignment of tasks to machines to make this assignment valid. Also, there are so-called soft constraints (again, see below) that result in penalties for an assignment and the goal of the systems is to find the assignment of tasks to machines that has the minimal penalty value (if such an assignment exists).

Let us first take a look at the different types of hard and soft constraints before we define the structure of an input file for this problem and the required output formats.

Hard constraints:

forced partial assignment: 
this hard constraint consists of up to 8 pairs (mach,task), with mach in {1,2,3,4,5,6,7,8} and task in {A,B,C,D,E,F,G,H}. Any assignment, that for any machine mach in one of the pairs does not assign the indicated task task to this machine is invalid. 
Error handling: if among the pairs are two pairs with the same machine or two pairs with the same task, then the system should output "partial assignment error" and stop execution.
forbidden machine: 
this hard constraint consists of a list of pairs (mach,task), with mach in {1,2,3,4,5,6,7,8} and task in {A,B,C,D,E,F,G,H}. Any assignment, that assigns to a machine mach a task task that is in this list is invalid.
too-near tasks: 
this hard constraint consists of a list of pairs (task1,task2), with task1,task2 in {A,B,C,D,E,F,G,H}. Any assignment that assigns task1 to a machine i and task2 to machine i+1 (or task1 to machine 8 and task2 to machine 1) is invalid.
For all these hard constraints, if in their description occurs either a machine that is not in {1,2,3,4,5,6,7,8} or a task that is not in {A,B,C,D,E,F,G,H}, then the system should terminate with the message "invalid machine/task".
Soft constraints:

machine penalties:
this soft constraint is entered in the form of 8 lines each containing 8 natural numbers (including 0). If p is the number at position i on line j, then p is added to the penalty value of an assignment, if this assignment has assigned to machine j the task that is on the i-th position in A,B,C,D,E,F,G,H (so, the lines represent the machines and the columns represent the tasks).
Problem handling: if there are more or less than 8 lines (of the form described above) or if one of the lines does not have enough numbers, then the system should output "machine penalty error" and stop execution.
too-near penalities:
this soft constraint is represented as a list of triples (task1,task2,p), with task1, task2 in {A,B,C,D,E,F,G,H} and p a natural number. p is added to the penalty value of an assignment, if this assignment has assigned task1 to a machine i and task2 to machine i+1 (or task1 to machine 8 and task2 to machine 1).
Problem handling: if in any of the triples occurs a task that is not in {A,B,C,D,E,F,G,H}, then the system should terminate with the message "invalid task". If we have two triples (task1,task2,p1) and (task1,task2,p2) then the pi that appears last is the penality value to use.
The overall penalty value of an assignment is the sum of all soft constraint penalties from above.
For all these soft constraints, if the system expects a natural number and the input does not provide one, then the system should terminate with the message "invalid penalty".

Note that too-near tasks and too-near penalities essentially deal with the same "problem", namely not wanting certain tasks on "neighboring" machines. But while using a hard constraint might result in not being able to get any valid assignment (which sometimes is what we want), using a soft constraint allows to distinguish how "bad" the problem is for different task pairs.

With these constraints, the general structure of an input file for your system is as follows:

Name:
*name*

forced partial assignment:
(*mach1*,*task1*)
...
(*machn*,*taskn*)

forbidden machine:
(*mach'1*,*task'1*)
...
(*mach'm*,*task'm*)
 
too-near tasks:
(*task"11*,*task"12*)
...
(*task"k1*,*task"k2*)

machine penalties:
*p11* *p12* *p13* *p14* *p15* *p16* *p17* *p18*
*p21* *p22* *p23* *p24* *p25* *p26* *p27* *p28*
*p31* *p32* *p33* *p34* *p35* *p36* *p37* *p38*
*p41* *p42* *p43* *p44* *p45* *p46* *p47* *p48*
*p51* *p52* *p53* *p54* *p55* *p56* *p57* *p58*
*p61* *p62* *p63* *p64* *p65* *p66* *p67* *p68*
*p71* *p72* *p73* *p74* *p75* *p76* *p77* *p78*
*p81* *p82* *p83* *p84* *p85* *p86* *p87* *p88*

too-near penalities
(*task+11*,*task+12*,*p1*)
...
(*task+x1*,*task+x2*,*px*)

Here, *name* is any non-empty character string that does not contain a blank, *machi*, *mach'i*, are out of {1,2,3,4,5,6,7,8}, *taski*, *task'i*, *task"ij*, *task+ij* are out of {A,B,C,D,E,F,G,H} and all *pi* and *pij* are natural numbers (or 0). n is less or equal to 8. If n, m, k, or x are 0, then the file will just contain the key word for the particular constraint and no further lines (for that constraint). If any key word is missing, one of the place holders above is assigned an invalid value, or the file contains something not specified above, output "Error while parsing input file" (except if we already defined a different error message above).

Your systems should be to handle any additional blanks at the end of any line in the input file (handle meaning that the system should ignore them) and also any additional empty lines between lines.
Your systems have to read their input from file and also write all output to a file. So, if your system is called mysystem, the input file is myinput and the file for the output is myoutput, the command for running your system should be:

       mysystem myinput myoutput

We will use scripts for performing our tests and therefore need all systems to strictly follow all requirements given above and below!

While, in theory, it is still possible to solve this problem by simply enumerating the 40320 possible assignments of tasks to machines, scheduling problems are often solved by using a so-called branch-and-bound method. Regardless of the solution method, if there are no problems in the input file/problem instance specification, your system should output
"No valid solution possible!"
if there is no solution that fulfills the given hard constraints or 
"Solution" *task*1 *task*2 *task*3 *task*4 *task*5 *task*6 *task*7 *task*8"; Quality:" *qual*
where *task*i in {A,B,C,D,E,F,G,H}, *task*i is the task assigned to machine i in the best assignment, i.e. the assignment with minimal overall penalty value and *qual* is this overall penalty value of the assignment.
If there are several assignments with the same minimal overall penalty value, any of these assignments will be an acceptable output.
