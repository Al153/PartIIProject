import json
import csv
import sys

# process data from here https://www.kaggle.com/tmdb/tmdb-movie-metadata

class Movie(object):
	"""Movie object"""
	def __init__(self, id, title, cast, directors):
		self.id = id
		self.title = title
		self.cast = cast
		self.directors = directors

	def toNode(self):
		return {
			"title": self.title,
			"_key": str(self.id),
			"language": "english",
			"type": "Movie"
		}

		
class Person(object):
	"""actor object"""
	def __init__(self, name, id):
		self.name = name
		self.id = id

	def __eq__(self, that):
		return isinstance(that, self.__class__) and self.name == that.name and self.id == that.id

	def __hash__(self):
   		return hash(self.name) ^ (hash(self.id) << 1)

	def toNode(self):
		return {
			"name": self.name,
			"_key": str(self.id),
			"type": "Person"
		}

class ActsIn(object):
	"""docstring for ActsIn"""
	def __init__(self, person, movie):
		self.person = person
		self.movie = movie

	def toNode(self):
		return {
			"_key": str(0),
			"_from": str(self.person.id),
			"_to": str(self.movie.id), 
			"$label": "ACTS_IN" 
		}

		

class Directed(object):
	"""docstring for ActsIn"""
	def __init__(self, person, movie):
		self.person = person
		self.movie = movie

	def toNode(self):
		return {
			"_key": str(0),
			"_from": str(self.person.id),
			"_to": str(self.movie.id), 
			"$label": "DIRECTED" 
		}
		
	

def getNMovies(file, n, lowDensity):
	r = csv.reader(file)
	columns = r.next()
	movies = []
	for row in r:
		movies.append(Movie(row[0], row[1], getActors(json.loads(row[2])), getDirectors(json.loads(row[3]))))
	return sorted(movies, key = lambda m: len(m.cast), reverse = not(lowDensity))[:n]

def getActors(cast):
	actors = []
	for actor in cast:
		newRole = Person(actor['name'], actor['id'])
		actors.append(newRole)
	return actors

def getDirectors(crew):
	directors = []
	for director in (filter(lambda c: c['job'] == "Director", crew)):
		newDirector = Person(director['name'], director['id'])
		directors.append(newDirector)
	return directors


def getActedIn(movies):
	actedIn = []
	for movie in movies:
		actedIn += map(lambda a: ActsIn(a, movie), movie.cast)
	return actedIn

def getDirected(movies):
	directed = []
	for movie in movies:
		directed += map(lambda d: Directed(d, movie), movie.directors)
	return directed

def multilineJson(jString):
	return jString.replace("}, ", "},\n")

def doMain(sourceFile, n, destinationFolderPath, lowDensity = False):
	movies = getNMovies(sourceFile, n, lowDensity)
	print "Got movies"
	people = list(set([person for movie in movies for person in (movie.cast + movie.directors)]))
	print "Got People"
	actedIn = getActedIn(movies)
	print "got acted in"
	directed = getDirected(movies)
	print "got directed"
	
	nodes = json.dumps([p.toNode() for p in people] + [m.toNode() for m in movies])
	print "got nodes json"
	edges = json.dumps([r.toNode() for r in actedIn] + [r.toNode() for r in directed])
	print "got edges json"
	
	open(destinationFolderPath + "/nodes.json", "w").write(multilineJson(nodes))
	open(destinationFolderPath + "/edges.json", "w").write(multilineJson(edges))


if __name__ == "__main__":
	# Usage: prep.py <dest> <n>
	doMain(open("tmdb_5000_credits.csv"), int(sys.argv[2]), sys.argv[1])
