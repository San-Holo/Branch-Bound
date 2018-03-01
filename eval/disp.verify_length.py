#!/usr/bin/env python
import sys
import collections

# Here, a machine is a deque with pending tasks and a task is a list of the time remaining for each machine.

def is_finished(lq):
    vd = collections.deque([])
    for q in lq:
        if q != vd:
            return False
    return True

def round(n_machines, l_machines):
    is_empty = [] # not to perform two rounds in one in case of starting with 0
    for i in range(0, n_machines):
        is_empty.append(l_machines[i] == collections.deque([]))

    for i in range(0, n_machines):
        #if not(is_empty[i]):
            
        while(not(is_empty[i]) and l_machines[i] != collections.deque([]) and l_machines[i][0][i] <= 0):
            task_temp = l_machines[i].popleft()
            is_empty[i] = (l_machines[i] == collections.deque([]))
            if (i < n_machines-1):
                l_machines[i+1].append(task_temp)
                is_empty[i] = (l_machines[i] == collections.deque([]))

        if (not(is_empty[i])):
            print("i=" + str(i))
            print(l_machines)
            l_machines[i][0][i]-=1

        while(not(is_empty[i]) and l_machines[i] != collections.deque([]) and l_machines[i][0][i] <= 0):
            task_temp = l_machines[i].popleft()
            is_empty[i] = (l_machines[i] == collections.deque([]))
            if (i < n_machines-1):
                l_machines[i+1].append(task_temp)
                is_empty[i] = (l_machines[i] == collections.deque([]))
                    
    print("is_empty: " + str(is_empty))
    print("l_machines: " + str(l_machines))

if __name__ == "__main__":
    f_solu = open(sys.argv[1])
    l_lines = list(f_solu)
    n_tasks = int(l_lines.pop(0))

    # TODO: catch exceptions
    l_tasks = list(map(list, zip(*[[int(i) for i in line.split()] for line in l_lines])))
    
    if (n_tasks != len(l_tasks)):
        print("Error: n_tasks is not equal to len(l_tasks).")
        print("n_tasks: " + str(n_tasks))
        print("l_tasks:\n" + str(l_tasks))
        exit(1)

    print("l_tasks: " + str(l_tasks))
    
    l_machines = [collections.deque(l_tasks)]
    n_machines = len(l_tasks[0])
    for i in range(1, n_machines):
        l_machines+=[collections.deque([])]
        
    print("l_machines: " + str(l_machines))

    print
    
    rounds=0
    while(not(is_finished(l_machines))):
        print("round number " + str(rounds) + ":")
        rounds+=1
        round(n_machines, l_machines)
        
        print

    print("****************")
    print("Rounds: " + str(rounds))
    print("****************")
