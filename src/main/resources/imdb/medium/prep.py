import json
import csv

# process data from here

def getMovies(file):
	with csv.reader(file) as r:
		columns = r.next()
		print columns
		movies = []
		for row in r:
			movies.append({'id': row[0], 'title': row[1], 'cast': json.loads(row[2])})