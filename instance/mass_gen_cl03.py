#!/usr/bin/env python3

# génération de données corrélées selon les tâches

import sys
import random

if __name__ == "__main__":
    if (len(sys.argv) > 4 or len(sys.argv) <= 0):
        print()
        print("Error: wrong number of arguments.")
        print("sys.argv[1]: number of instances (default: 100)")
        print("sys.argv[2]: number of tasks (default: 100)")
        print("sys.argv[3]: number of machine (default: 3)")
        print()
        exit(1)

    CLASS_ID = 'c'
    N_INSTANCES = 100
    N_TASKS = 100
    N_MACHINES = 3

    if (len(sys.argv) >= 2):
        N_INSTANCES = int(sys.argv[1])
    if (len(sys.argv) >= 3):
        N_TASKS = int(sys.argv[2])
    if (len(sys.argv) >= 4):
        N_MACHINES = int(sys.argv[3])

    for n_inst in range(0, N_INSTANCES):
        filename = CLASS_ID + str(N_TASKS) + '_' + str(n_inst)
        f = open(filename, 'w')

        f.write(str(N_TASKS) + "\n")

        for x in range(0, N_MACHINES):
            l_tmp = []
            a = 15*x+1
            b = 15*x+100
            for j in range(0, N_TASKS):
                l_tmp.append(random.randrange(a, b+1))
            f.write(' '.join(str(v) for v in l_tmp))
            f.write('\n')
    
        f.close()
    # end for n_inst in range(0, N_INSTANCES)
