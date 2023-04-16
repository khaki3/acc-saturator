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

plt.rcParams["figure.figsize"] = (8,3.5)
plt.rcParams['text.usetex'] = True
plt.rcParams['text.latex.preamble'] = "\\usepackage{libertine}"
plt.rcParams['ps.usedistiller'] = "xpdf"

def extract_log(path):
    r=[]
    sum=0
    for line in open(path).readlines():
        x=(line.rstrip('\n').split(","))
        sum+=float(x[0])
        r.append([len(r), float(x[1]), float(x[2]), float(x[3]), sum, float(x[4])])
    return r

log=np.array(extract_log("bt-breakdown-gcc.log")).transpose()

sns.set_theme()
sns.set('talk', 'whitegrid', 'dark', font_scale=1.5,
        rc={"lines.linewidth": 2, 'grid.linestyle': '--'})

fig, ax = plt.subplots()

# print(log[0])
# print(log[1])
# print(log[2])
# print(log[3])
# print(log[5])

ax.scatter(log[0], log[1], marker=".", label="CSE",   s=300, c="red", clip_on=False, zorder=1)
ax.scatter(log[0], log[2], marker=".", label="+SAT",  s=300, c="black", clip_on=False, zorder=2)
ax.scatter(log[0], log[3], marker="p", label="+BULK", s=100, c="green", clip_on=False, zorder=4)
ax.scatter(log[0], log[5], marker="p", label="-SAT", s=100, c="m", clip_on=False, zorder=3)

# plt.legend(borderaxespad=0, fontsize=18)
# sns.move_legend(ax, "upper left", bbox_to_anchor=(0, 1))
# plt.legend(ncol=3)

# ax.tick_params(axis='x', labelsize=20)
ax.tick_params(axis='y', labelsize=20)
# ax.set_yticks([0.75, 1.00, 1.25, 1.50, 1.75, 2.00, 2.25], [0.75, "1.00", 1.25, "1.50", 1.75, "2.00", 2.25])
# ax.set_xticks([25, 50, 75, 100, 125, 150], [25, 50, 75, 100, 125, 150])
ax.set_yticks([0.75, 1, 2, 3, 4, 5, 6], ["", "", "2.00", "3.00", "4.00", "5.00", "6.00"])
ax.set_ylim([0.75, 6])
ax.set_xlim([0, 45])
ax.set_ylabel('Speedup', fontsize=24)
# plt.xlabel('Register/Thread', fontsize=24)
ax.set_xticks([])
ax.grid(axis='y', linestyle='dashed', linewidth = 2)
plt.text(-3.4, 0.5, "0.75", fontsize=20, ha='center', va='bottom')
plt.text(-3.4, 1, "1.00", fontsize=20, ha='center', va='bottom')

ax2=ax.twinx()
ax2.get_xaxis().set_visible(False)
ax.patch.set_visible(False)
ax.zorder=2
ax2.zorder=1
ax2.fill_between(np.append(log[0],[log[0][-1]+1,log[0][-1]+2]), np.append(log[4],[100, 100]), 0, where= np.append(log[4],[100, 100]) > 0, facecolor='#D3CF8F', alpha=0.5, interpolate=True)
# ax2.plot(np.append(log[0],[log[0][-1]+1,log[0][-1]+2]), np.append(log[4],[log[4][-1],log[4][-1]]), clip_on=False, linewidth=4, linestyle='--', dashes=(4.1, 1), c='sandybrown')
ax2.set_ylim([0, 100])
ax2.set_ylabel('Ratio (\%)', fontsize=24)
ax2.tick_params(axis='y', labelsize=20)
ax2.grid(axis='y', linestyle='None')
ax2.set_yticks([0, 25, 50, 75, 100], [0, 25, 50, 75, 100])

ax.set_axisbelow(True)
ax2.set_axisbelow(True)

sns.despine(bottom = True, left = True)

plt.tight_layout()
plt.savefig('bt-breakdown-gcc.pdf', bbox_inches='tight')
