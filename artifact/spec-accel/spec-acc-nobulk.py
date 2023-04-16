import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
import matplotlib.ticker as plticker
from matplotlib.ticker import FormatStrFormatter
from matplotlib.colors import colorConverter as cc
import seaborn as sns
import numpy as np
import sys
import os
import re
import glob

plt.rcParams["figure.figsize"] = (10,3)
plt.rcParams['text.usetex'] = True
plt.rcParams['text.latex.preamble'] = "\\usepackage{libertine}"
plt.rcParams['ps.usedistiller'] = "xpdf"

sns.set_theme()
fig, ax = plt.subplots()

def extract(path):
    return np.array([int(x) for x in open(path).readlines()[0].rstrip('\n').split(",")])

nvhpc=extract("nvhpc.log")[0:7]
nvhpc_sat=extract("nvhpc-nobulk.log")[0:7]
gcc=extract("gcc.log")[0:7]
gcc_sat=extract("gcc-nobulk.log")[0:7]

names=["ostencil", "olbm", "omriq", "ep", "cg", "csp", "bt"]
xstep=np.arange(len(nvhpc))
fig, ax = plt.subplots()
plt.bar(xstep-0.225, nvhpc/nvhpc_sat, color="#B3E680", width=0.45, edgecolor='black', alpha=0.5)
plt.bar(xstep+0.225, gcc/gcc_sat, color="#D3CF8F", width=0.45, edgecolor='black', alpha=0.5)

pad=-0.02
for bars in ax.containers:
    # ax.bar_label(bars, fmt='%10.2fx', fontsize=24)

    for rect in bars:
        height = rect.get_height()
        label = '%.2fx' % height
        plt.text(rect.get_x() + rect.get_width()/2.0 + pad, height, label, fontsize=20, ha='center', va='bottom')
    pad+=0.02

#80E5E7

ax.tick_params(axis='y', labelsize=24)
ax.set_yticks([0, 1, 2], [0, 1, 2]) # Bold fond
plt.ylabel('Speedup', fontsize=28)
ax.set_xticks(xstep, names, fontsize=26, rotation=0) 

plt.ylim([0,2.2])

plt.tight_layout()
plt.grid(axis='y', linestyle='dashed', linewidth = 2)
plt.savefig('spec-acc-cse+sat.pdf', bbox_inches='tight', pad_inches=0)
