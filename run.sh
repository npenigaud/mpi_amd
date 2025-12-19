#!/bin/bash
##SBATCH -N2
#SBATCH -N1
#SBATCH --time 00:01:00
#SBATCH --export="NONE"
#SBATCH --partition MI300X
##SBATCH --gres=gpu:1
#SBATCH --gres=gpu:2
#SBATCH --mem=30G 
##SBATCH --mem=12G
#SBATCH -c 2

source /home/afar/modules/use.sh
module load rocm
module load afar/22.2.0
module load openmpi-rt/ucx
module load openmpi/afar-22.2.0/5.0.8
module unload -f openmpi-rt/ucx
module load openmpi-rt/sm


#source /usr/local/apps/hpc_sdk/22.3/Linux_x86_64/2022/comm_libs/hpcx/latest/hpcx-mt-init.sh hpcx_load#!/bin/bash

set -x

ulimit -s unlimited
export OMP_STACK_SIZE=16G

mpirun -n 2 --oversubscribe ./mpc.x
