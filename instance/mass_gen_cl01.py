#!/usr/bin/env python3

# génération de données non corrélées

import sys
import random

if __name__ == "__main__":
    if (len(sys.argv) > 6 or len(sys.argv) <= 0):
        print()
        print("Error: wrong number of arguments.")
        print("sys.argv[1]: number of instances (default: 100)")
        print("sys.argv[2]: number of tasks (default: 100)")
        print("sys.argv[3]: min time on a machine (default: 1)")
        print("sys.argv[4]: max time on a machine (default: 100)")
        print("sys.argv[5]: number of machine (default: 3)")
        print()
        exit(1)

    CLASS_ID = 'a'
    N_INSTANCES = 100
    N_TASKS = 100
    N_START = 1
    N_STOP = 100
    N_MACHINES = 3
    N_STEP = 1

    if (len(sys.argv) >= 2):
        N_INSTANCES = int(sys.argv[1])
    if (len(sys.argv) >= 3):
        N_TASKS = int(sys.argv[2])
    if (len(sys.argv) >= 4):
        N_START = int(sys.argv[3])
    if (len(sys.argv) >= 5):
        N_STOP = int(sys.argv[4])
    if(N_START > N_STOP):
        print("Error: N_START > N_STOP")
        exit(1)
    if (len(sys.argv) >= 6):
        N_MACHINES = int(sys.argv[5])

    for n_inst in range(0, N_INSTANCES):
        filename = CLASS_ID + str(N_TASKS) + '_' + str(n_inst)
        f = open(filename, 'w')
        f.write(str(N_TASKS) + "\n")

        for i in range(0, N_MACHINES):
            l_tmp = []
            for j in range(0, N_TASKS):
                l_tmp += [random.randrange(N_START, N_STOP+1, N_STEP)]
            f.write(' '.join(str(i) for i in l_tmp))
            f.write('\n')

        f.close()
    # end for n_inst in range(0, N_INSTANCES)
