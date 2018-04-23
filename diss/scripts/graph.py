import numpy as np
import matplotlib.pyplot as plt
import math




def autolabel(rects, ax):
    """
    Attach a text label above each bar displaying its height
    """
    for rect in rects:
        height = rect.get_height()
        ax.text(rect.get_x() + rect.get_width()/2., 1.05*height,
                '%d' % int(height),
                ha='center', va='bottom')

###########################################
# Project data

class TestResult(object):
	"""docstring for TestResult"""
	def __init__(self, name, ref, original = None, batched = None, cse = None, postgres = None):
		super(TestResult, self).__init__()
		self.name = name
		self.ref = ref
		self.original = original
		self.batched = batched
		self.cse = cse
		self.postgres = postgres

		self.refName = "Reference"
		self.cseName = "LMDB CSE"
		self.postgresName = "Postgres"
		self.batchedName = "LMDB Batched"
		self.origName = "LMDB original"

		self.refColour = 'r'
		self.cseColour = 'g'
		self.postgresColour = 'b'
		self.batchedColour = 'c'
		self.origColour = 'y'

	def __getValues(self):
		names = [self.refName]
		values = [self.ref]
		colours = [self.refColour]

		if self.original != None:
			names.append(self.origName)
			values.append(self.original)
			colours.append(self.origColour)

		if self.batched != None:
			names.append(self.batchedName)
			values.append(self.batched)
			colours.append(self.batchedColour)

		if self.cse != None:
			names.append(self.cseName)
			values.append(self.cse)
			colours.append(self.cseColour)

		if self.postgres != None:
			names.append(self.postgresName)
			values.append(self.postgres)
			colours.append(self.postgresColour)

		return (names, values, colours)



	def plot(self):
		fig, ax = plt.subplots()
		(names, values, colours) = self.__getValues()
		ind = np.arange(len(names))
		width = 0.35
		rects = ax.bar(ind, values, width, color='r')
		ax.set_ylabel("Time to Execute (ms)")
		ax.set_title("Back-End Performance on  "+ self.name)
		ax.set_xticks(ind + width/2)
		ax.set_xticklabels(names)
		ax.set_xlim(left=-0.25, right=max(ind)+0.75)
		plt.grid(True)
		for i in range(len(colours)):
			rects[i].set_color(colours[i])
		plt.savefig(self.name + ".png")

	def plotLog(self):
		fig, ax = plt.subplots()
		(names, values, colours) = self.__getValues()
		values = map(lambda x: math.log(x), values)
		ind = np.arange(len(names))
		width = 0.35
		rects = ax.bar(ind, values, width, color='r')
		ax.set_ylabel("Log Time to Execute")
		ax.set_title("Back-End Performance on  "+ self.name)
		ax.set_xticks(ind + width/2)
		ax.set_xticklabels(names)
		ax.set_xlim(left=-0.25, right=max(ind)+0.75)
		plt.grid(True)

		for i in range(len(colours)):
			rects[i].set_color(colours[i])
		#autolabel(rects, ax)
		plt.savefig(self.name + "Log.png")

class TrendResult(object):
	def __init__(self, name, ref, original = None, batched = None, cse = None, postgres = None):
		super(TrendResult, self).__init__()
		self.name = name
		self.ref = ref
		self.original = original
		self.batched = batched
		self.cse = cse
		self.postgres = postgres

		self.refName = "Reference"
		self.cseName = "LMDB CSE"
		self.postgresName = "Postgres"
		self.batchedName = "LMDB Batched"
		self.origName = "LMDB original"

		self.refColour = 'ro-'
		self.cseColour = 'gv-'
		self.postgresColour = 'b^-'
		self.batchedColour = 'cs-'
		self.origColour = 'yp-'

	def __getValues(self):
		names = [self.refName]
		values = [self.ref]
		colours = [self.refColour]

		if self.original != None:
			names.append(self.origName)
			values.append(self.original)
			colours.append(self.origColour)

		if self.batched != None:
			names.append(self.batchedName)
			values.append(self.batched)
			colours.append(self.batchedColour)

		if self.cse != None:
			names.append(self.cseName)
			values.append(self.cse)
			colours.append(self.cseColour)

		if self.postgres != None:
			names.append(self.postgresName)
			values.append(self.postgres)
			colours.append(self.postgresColour)

		return (names, values, colours)

	def plot(self, logCoeff = 0):
		fig, ax = plt.subplots()
		(names, values, colours) = self.__getValues()
		width = 0.35
		lines = []
		for i in xrange(len(values)):
			xs = list(values[i][0].keys())
			ys = [values[i][0][j] for j in xs]
			err = values[i][1]
			if err:
				errbars = [err[j] for j in xs]
				lines.append(ax.plot(xs, ys, colours[i], label = names[i]))
				plt.errorbar(xs, ys, yerr=errbars, linestyle="None")
			else:
				lines.append(ax.plot(xs, ys, colours[i], label = names[i]))
		if logCoeff:
			import math
			xs = filter(lambda x: x > 0, list(values[0][0]))
			ys = [math.log(x, 2)*logCoeff for x in xs]
			lines.append(ax.plot(xs, ys, "k--", label = str(logCoeff)+"*log2(n)"))
		ax.set_ylabel("Time to Execute (ms)")
		ax.set_xlabel("Repetitions")
		ax.set_title("Back-End Performance on  "+ self.name)
		plt.grid(True)
		plt.legend()
		plt.savefig(self.name + "Trend.png")

        def plotLog(self):
                fig, ax = plt.subplots()
                (names, values, colours) = self.__getValues()
                width = 0.35
                lines = []
		import math
                for i in xrange(len(values)):
                        xs = list(values[i][0].keys())
                        ys = [math.log(values[i][0][j], 2) for j in xs]
                        err = values[i][1]
                        lines.append(ax.plot(xs, ys, colours[i], label = names[i]))

                ax.set_ylabel("Log time to Execute")
                ax.set_xlabel("Repetitions")
                ax.set_title("Log Back-End Performance on  "+ self.name)
                plt.grid(True)
                plt.legend()
                plt.savefig(self.name + "LogTrend.png")


def mean(data):
    """Return the sample arithmetic mean of data."""
    n = len(data)
    if n < 1:
        raise ValueError('mean requires at least one data point')
    return sum(data)/float(n)

def _ss(data):
    """Return sum of square deviations of sequence data."""
    c = mean(data)
    ss = sum((x-c)**2 for x in data)
    return ss

def stdev(data, ddof=0):
    """Calculates the population standard deviation
    by default; specify ddof=1 to compute the sample
    standard deviation."""
    n = len(data)
    if n < 2:
        raise ValueError('variance requires at least two data points')
    ss = _ss(data)
    pvar = ss/(n-ddof)
    return pvar**0.5


def getRegularTrendData(fname):
	f = open(fname)
	res = {}
	for i, line in enumerate(f):
		res[i+1] = int(line)
	return (res, None)

def getModularTrendData(fname, modulus):
	f = open(fname)
	res = {}
	for i, line in enumerate(f):
		i_mod = (i+1)%modulus
		if (i_mod) in res:
			res[i_mod].append(int(line))
		else:
			res[i_mod] = [int(line)]

	values = {i:mean(l) for (i,l) in res.items()}
	ebars = {i:stdev(l) for (i,l) in res.items()}
	return (values, ebars)

def plotBimodal(cv):
	fig, ax = plt.subplots()
	data = [633,633,641,639,637,634,625,642,629,641,640,629,2177,686,624,632,624,651,628,631,637,626,639,634,623,628,2456,668,646,646,636,2606,632,640,627,630,643,640,658,648,633,625,628,624,634,654,630,638,633,640,2422,635,630,625,629,2636,623,625,631,646,648,626,645,642,625,632,626,630,628,633,631,636,641,669,627,626,632,670,681,692,626,634,632,634,2191,624,629,631,636,2484,627,638,633,626,2532,628,627,635,631,2611,628,627,628,662,705,633,622,625,636,664,624,636,624,624,635,640,631,628,643,626,628,650,632,626,631,622,639,629,627,640,626,635,625,681,625,632,634,633,631,626,624,639,647,2615,627,636,647,625,2383,634]
	import numpy as np
	from scipy.stats import gaussian_kde
	density = gaussian_kde(data)
	xs = np.linspace(0,3000,1000)
	density.covariance_factor = lambda : cv
	density._compute_covariance()
	plt.plot(xs,density(xs))
	ax.set_ylabel("Density")
	ax.set_xlabel("JoinSpeed 'And' query Latency (ms)")
	ax.set_title("Latency Density for LMDB Optimised")

	plt.savefig("LatencyDensity.png")



def plotJoinSpeed(ref, cse, batched, postgres):
	fig, ax = plt.subplots()
	names = ["Ref (And)", "Ref (Join)" , "CSE (And)", "CSE(Join)", "Batched (And)", "Batched (Join)", "Postgres (And)", "Postgres (Join)"]
	values = ref + cse  + batched + postgres
	colours = "rrggccbb"
	ind = np.arange(len(names))+1
	width = 0.35
	rects = ax.bar(ind, values, width, color='r')
	ax.set_ylabel("Median Time to Execute")
	ax.set_title("Back-End performance on JoinSpeed queries")
	ax.set_xticks(ind + width/2)
	ax.set_xticklabels(names)
	ax.set_xlim(left=0, right=max(ind)+0.75)
	plt.xticks(rotation=45)

	plt.grid(True)

	for i in range(len(colours)):
		rects[i].set_color(colours[i])
		#autolabel(rects, ax)
	plt.savefig("joinspeedComp.png")



if __name__ == '__main__':
	from matplotlib import rcParams
	rcParams.update({'figure.autolayout': True})

	redundancy = TestResult("Redundancy", ref = 381972, cse = 379280, batched = 9318741)
	conjunctions = TestResult("Intersections", ref = 879984, cse = 827734, batched = 791678, postgres = 106165)
	disjunctions = TestResult("Unions", ref = 853664, cse = 824799, batched = 800455, postgres = 112488)
	exactly = TestResult("ExactlyTest", ref = 1752, cse = 1798, batched = 1513, postgres = 102461)
	exactlyPairs = TestResult("ExactlyPairs", ref = 83269, cse = 1792388, batched = 1787390, postgres = 107797)
	upto = TestResult("Upto", ref = 126448, cse = 28491910, batched = 28423658, postgres = 197880)
	uptoLarge = TestResult("UptoLarge", ref = 1059091, postgres = 1557314)
	joinSpeed = TestResult("JoinSpeed", ref = 368320, cse = 525992, batched = 543909, postgres = 289352)
	from os.path import join
	uptoTrend = TrendResult("Upto",
		ref =  getRegularTrendData(join("trends", "uptoRef.dat")),
		cse =  getRegularTrendData(join("trends", "uptoCSE.dat")),
		postgres =  getRegularTrendData(join("trends", "uptoSQL.dat")),
		batched = getRegularTrendData(join("trends", "uptoBatched.dat"))
	)
	uptoLargeTrend = TrendResult("UptoLarge",
                ref =  getModularTrendData(join("trends", "ULargeRef.dat"), 10),
                postgres =  getModularTrendData(join("trends", "ULargeSQL.dat"), 10)
	)
	redundancy.plot()
	conjunctions.plot()
	disjunctions.plot()
	exactly.plot()
	exactly.plotLog()
	exactlyPairs.plot()
	exactlyPairs.plotLog()
	upto.plot()
	upto.plotLog()
	uptoLarge.plot()
	joinSpeed.plot()

	uptoTrend.plot()
	uptoTrend.plotLog()
	uptoLargeTrend.plot(5500)

	plotBimodal(0.05)


	plotJoinSpeed([633, 1443], [884, 2285], [953, 2028], [345, 1557])
