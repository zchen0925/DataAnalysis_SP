import os
#os.chdir("/Dropbox/Emily.Marina/SPR_Surprisal_Analysis/")

import pandas as pd
import numpy as np
import math
import time
import statistics
import getngrams
import pickle

# set year you want to select (2019=219)
startyr = 217
endyr = 224
new_dict = True
freq0_words = []

# either start anew or open old dict
if new_dict:
	wordDict = {}
else:
	with open('wordDict.pickle', 'rb') as f:
		wordDict = pickle.load(f)	

# read in csv with sentences
gp_sentencepairs = np.genfromtxt("/Users/ziqichen/Desktop/SPR_Surprisal_Analysis/gp_ngp_surprisal.csv",delimiter = ',',skip_header=1,usecols=(2,5),dtype=str,encoding='utf-8-sig')
mv_sentencepairs = np.genfromtxt("/Users/ziqichen/Desktop/SPR_Surprisal_Analysis/mv_nmv_surprisal.csv",delimiter = ',',skip_header=1,usecols=(2,5),dtype=str,encoding='utf-8-sig')
sentencepairs = np.concatenate((gp_sentencepairs, mv_sentencepairs),axis=0)
# each row is a pair of sentences
for count,sentencepair in enumerate(sentencepairs):

	#if count == 2:
		#break
	
	print("Working on row %d\n"%count)

	# get individual sentences within pair
	for sentence in sentencepair:

		# get arr of words; remove period from last word
		words = sentence.split()
		words[len(words)-1] = words[len(words)-1].strip(".")
		words_freq = []

		for word in words: #freq0_words

			# if dict does not already contain this word, run query
			if wordDict.get(word) == None:
				
				# decide which word to query (issue w apostrophes)
				if word != "sister's" and word != "brother's" and word != "mother's" and word != "Now" and word != "women's" and word != "x_ray":
					query_word = word
				elif word == "sister's":
					query_word = "sister"
				elif word == "brother's":
					query_word = "brother"
				elif word == "mother's":
					query_word = "mother"
				elif word == "Now":
					query_word = "now"
				elif word == "women's":
					query_word = "women"
				elif word == "x_ray":
					query_word = "xray"

				time.sleep(10)
				print("querying %s\n"%query_word)
				params = "%s -corpus=eng_us_2019 -endYear=2019"%(query_word)
				df = getngrams.runQuery(params)

				# subset df to 2019 and take log
				if not df.empty:
					prob = np.nanmean(df.iloc[startyr:endyr]["timeseries"])
					logprob = -math.log(prob)
					wordDict[word] = [prob,logprob]
				else:
					freq0_words.append(word)

# save dict
with open('wordDict.pickle', 'wb') as f:
	pickle.dump(wordDict, f)

# deal with 0 frequencies: get highest and lowest logprob
# turns out, i didn't have to deal with this, except for covid terms, which i assigned to cancer.
# highest logprob = most infrequent
logprob_vals = []
for val in wordDict.values():
	logprob_vals.append(val[1])
logprob_max = max(logprob_vals)
logprob_min = min(logprob_vals)

# example of how to modify dict for these special cases
# for covid, might make sense to use max disease value?
word = "x"
wordDict[word] = [np.nan, logprob_max]


