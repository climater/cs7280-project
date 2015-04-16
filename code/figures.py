import pandas
import seaborn
import numpy
from matplotlib import pyplot

df = pandas.read_csv("df.csv")
counts = df['hur.count'].copy()
del df['hur.count']
df.columns = [x.replace(".data", "") for x in df.columns]
seaborn.set(style="darkgrid")

rs = numpy.random.RandomState(33)

f, ax = pyplot.subplots(figsize=(9, 9))
cmap = seaborn.diverging_palette(220, 10, as_cmap=True)
seaborn.corrplot(df, annot=False, sig_stars=False,
             diag_names=False, cmap=cmap, ax=ax)
f.tight_layout()
pyplot.savefig("corr.png")

pyplot.close()
pyplot.figure(figsize=(9, 9))
seaborn.distplot(counts)
pyplot.xlim([0, 20])
pyplot.xlabel("Hurricanes")
pyplot.title("Histogram of Annual Hurricane Counts")
pyplot.savefig("histogram.png")