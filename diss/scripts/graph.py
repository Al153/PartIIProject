import numpy as np
import matplotlib.pyplot as plt
import math

# N = 5
# men_means = (20, 35, 30, 35, 27)
# men_std = (2, 3, 4, 1, 2)
# 
# ind = np.arange(N)  # the x locations for the groups
# width = 0.35       # the width of the bars
# 
# fig, ax = plt.subplots()
# rects1 = ax.bar(ind, men_means, width, color='r', yerr=men_std)
# 
# women_means = (25, 32, 34, 20, 25)
# women_std = (3, 5, 2, 3, 3)
# rects2 = ax.bar(ind + width, women_means, width, color='y', yerr=women_std)
# 
# # add some text for labels, title and axes ticks
# ax.set_ylabel('Scores')
# ax.set_title('Scores by group and gender')
# ax.set_xticks(ind + width / 2)
# ax.set_xticklabels(('G1', 'G2', 'G3', 'G4', 'G5'))
# 
# ax.legend((rects1[0], rects2[0]), ('Men', 'Women'))


def autolabel(rects, ax):
    """
    Attach a text label above each bar displaying its height
    """
    for rect in rects:
        height = rect.get_height()
        ax.text(rect.get_x() + rect.get_width()/2., 1.05*height,
                '%d' % int(height),
                ha='center', va='bottom')

# autolabel(rects1)
# autolabel(rects2)
# 
# plt.show()

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
		for i in range(len(colours)):
			rects[i].set_color(colours[i])
		#autolabel(rects, ax)
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

		for i in range(len(colours)):
			rects[i].set_color(colours[i])
		#autolabel(rects, ax)
		plt.savefig(self.name + "Log.png")

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

	plotBimodal(0.05)