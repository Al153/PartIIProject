import json
import csv
import sys

# https://www.kaggle.com/cformey24/ufc-fights-data-1993-2232016/data#

class Fight(object):
	"""docstring for Fight"""
	def __init__(self, row):
		self.f1 = row["f1name"]
		self.f2 = row["f2name"]
		self.f1Won = row["f1result"] == "win"
		self.f2Won = row["f2result"] == "win"
		self.isDraw = not(self.f1Won or self.f2Won)

		self.winner = self.f1 if self.f1Won else self.f2
		self.loser = self.f2 if self.f1Won else self.f1

class FighterRecord(object):
	def __init__(self, row):
		self.name = row["name"]
		self.height = int(row["height"])
		self.weight = int(row["weight"])

	def toJson(self):
		return {
			"name": self.name,
			"height": self.height,
			"weight": self.weight
		}

class ShorterThan(object):
	def __init__(self, left, right):
		self.shorter = left
		self.taller = right

	def toJson(self):
		return {
			"label": "ShorterThan",
			"shorter": self.shorter.toJson(),
			"taller": self.taller.toJson()
		}
	
class LighterThan(object):
	"""docstring for LighterThan"""
	def __init__(self, left, right):
		self.lighter = left
		self.heavier = right		

	def toJson(self):
		return {
			"label": "LighterThan",
			"lighter": self.lighter.toJson(),
			"heavier": self.heavier.toJson()
		}

class Beat(object):
	def __init__(self, left, right):
		self.winner = left
		self.loser = right

	def toJson(self):
		return {
			"label": "Beat",
			"winner": self.winner.toJson(),
			"loser": self.loser.toJson()
		}



def getFights(file):
	r = csv.DictReader(file)
	fights = [Fight(row) for row in r]
	wonFights = filter(lambda f: not(f.isDraw), fights)
	return wonFights

def getFighterRecords(file):
	r = csv.DictReader(file)
	records = [FighterRecord(row) for row in r if row["name"] != "" and row["height"] != "" and row["weight"] != ""]
	recordDict = {r.name: r for r in records}
	return recordDict



# gets the transitive reduction of the "shorterThan" relation
def getHeightReductions(fightersByHeight):
	relations = []
	lastFighter = fightersByHeight[0]
	firstWithHeight = fightersByHeight[0]
	fightersByHeight = fightersByHeight[1:]
	for fighter in fightersByHeight:
		if (fighter.height > lastFighter.height): # cycle has ended
			relations.append(ShorterThan(lastFighter, fighter))

			if lastFighter != firstWithHeight: # if there is a cycle of more than one, link up to the start
				relations.append(ShorterThan(lastFighter, firstWithHeight))

			lastFighter = fighter
			firstWithHeight = fighter

		elif fighter.height == lastFighter.height:
			relations.append(ShorterThan(lastFighter, fighter))
			lastFighter = fighter
		else: raise Exception("Non-sorted weight")
	return relations


		

# gets the transitive reduction of the "shorterThan" relation
def getWeightReductions(fightersByWeight):
	relations = []
	lastFighter = fightersByWeight[0]
	firstWithWeight = fightersByWeight[0]
	fightersByWeight = fightersByWeight[1:]
	for fighter in fightersByWeight:
		if (fighter.weight > lastFighter.weight): # cycle has ended
			relations.append(ShorterThan(lastFighter, fighter))

			if lastFighter != firstWithWeight: # if there was previously a cycle of more than one, link up to the start
				relations.append(ShorterThan(lastFighter, firstWithWeight))

			lastFighter = fighter
			firstWithWeight = fighter

		elif (fighter.weight == lastFighter.weight):
			relations.append(ShorterThan(lastFighter, fighter))
			lastFighter = fighter
		else: raise Exception("Non-sorted weight")
	return relations


def multilineJson(jString):
	return jString.replace("}, {", "},\n{")

def getBeatRelations(fights, fighterRecords):
	return [Beat(fighterRecords[fight.winner], fighterRecords[fight.loser]) for fight in fights]

fights = getFights(open("ufc_wins.csv"))
print "Fights = ", len(fights)
records = getFighterRecords(open("ufc_fighters.csv"))
fighters = [records[key] for key in records]
print "fighter length = ", len(records)

validFights = filter(lambda fight: fight.f1 in records and fight.f2 in records, fights)
print "validFights", len(validFights)

byHeight = sorted(fighters, key = lambda f: f.height)
byWeight = sorted(fighters, key = lambda f: f.weight)

heightRelations = getHeightReductions(byHeight)
weightRelations = getWeightReductions(byWeight)
beatRelations = getBeatRelations(validFights, records)

json_relations = json.dumps(map(lambda r: r.toJson(), heightRelations + weightRelations + beatRelations))

open("edges.json", "w").write(multilineJson(json_relations))