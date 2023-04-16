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

nvhpc=extract("nvhpc.log")[7:]
nvhpc_sat=extract("nvhpc-sat.log")[7:]
gcc=extract("gcc.log")[7:]
gcc_sat=extract("gcc-sat.log")[7:]
clang=extract("clang.log")
clang_sat=extract("clang-sat.log")


names=["postencil", "polbm", "pomriq", "pep", "pcg", "pcsp", "pbt"]
xstep=np.arange(len(nvhpc))
fig, ax = plt.subplots()
plt.bar(xstep-0.3, nvhpc/nvhpc_sat, color="#B3E680", width=0.3, edgecolor='black', alpha=0.5)
plt.bar(xstep, gcc/gcc_sat, color="#D3CF8F", width=0.3, edgecolor='black', alpha=0.5)
plt.bar(xstep+0.3, clang/clang_sat, color="#80E5E7", width=0.3, edgecolor='black', alpha=0.5)

ax.tick_params(axis='y', labelsize=24)
ax.set_yticks([0, 1, 2], ["", "", ""]) # Bold fond
plt.ylabel(' ', fontsize=28)
ax.set_xticks(xstep, names, fontsize=26, rotation=0)

plt.ylim([0,2.2])

plt.tight_layout()
plt.grid(axis='y', linestyle='dashed', linewidth = 2)


# for bars in ax.containers:
    # for rect in bars:
    #     rect.set_height(min(rect.get_height(), 2.2))
    # ax.bar_label(bars, labels=[f'{x:.2f}x' for x in bars.datavalues], rotation=90, fontsize=20)

for i, bars in enumerate(ax.containers):
    for j, rect in enumerate(bars):
        height = rect.get_height()
        label = '%.2fx' % height
        plt.text(rect.get_x() + rect.get_width()/2.0, (height if height < 2 else 2.14), label, fontsize=20, ha='center', va='bottom', rotation=90 if height<2 else 0)

plt.savefig('spec-omp-accsat.pdf', bbox_inches='tight', pad_inches=0)
