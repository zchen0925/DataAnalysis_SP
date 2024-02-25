import os
#os.chdir("/Desktop/SPR_Surprisal_Analysis/")
import pandas as pd
import numpy as np
import math
import time
import statistics
import getngrams
import pickle

# main stim
gp_freq, ng_freq, gp_comb_freq = [],[],[]
mv_freq, nmv_freq, mv_comb_freq = [],[],[]

# open dict
with open('wordDict.pickle', 'rb') as f:
	wordDict = pickle.load(f)

# read in csv with sentences
gp_sentencepairs = np.genfromtxt("/Users/ziqichen/Desktop/SPR_Surprisal_Analysis/gp_ngp_surprisal.csv",delimiter = ',',skip_header=1,usecols=(2,5),dtype=str,encoding='utf-8-sig')
gp_item_num = np.genfromtxt("/Users/ziqichen/Desktop/SPR_Surprisal_Analysis/gp_ngp_surprisal.csv",delimiter = ',',skip_header=1,usecols=(1),dtype=str,encoding='utf-8-sig')

mv_sentencepairs = np.genfromtxt("/Users/ziqichen/Desktop/SPR_Surprisal_Analysis/mv_nmv_surprisal.csv",delimiter = ',',skip_header=1,usecols=(2,5),dtype=str,encoding='utf-8-sig')
mv_item_num = np.genfromtxt("/Users/ziqichen/Desktop/SPR_Surprisal_Analysis/mv_nmv_surprisal.csv",delimiter = ',',skip_header=1,usecols=(1),dtype=str,encoding='utf-8-sig')

# each row is a pair of sentences
for count,sentencepair in enumerate(gp_sentencepairs):
	
	print("Working on row %d\n"%count)
	which_pair = 0

	# get individual sentences within pair
	for cond, sentence in enumerate(sentencepair):

		# get arr of words; remove period from last word
		words = sentence.split()
		words[len(words)-1] = words[len(words)-1].strip(".")
		words_freq = []

		# get word's logprob value
		for word in words:
			words_freq.append(wordDict[word][1])

		# append mean freq across words to appropriate array
		if not cond:
			gp_freq.append(np.nanmean(words_freq))
		else:
			ng_freq.append(np.nanmean(words_freq))

	gp_comb_freq.append(statistics.mean([gp_freq[count],ng_freq[count]]))

#  write results to csv
pd.DataFrame({'item_num':gp_item_num,'gp_freq': gp_freq, 'ng_freq': ng_freq, 'gp_comb_freq': gp_comb_freq}).to_csv("gp_wordfreq_results.csv")




# each row is a pair of sentences
for count,sentencepair in enumerate(mv_sentencepairs):
	
	print("Working on row %d\n"%count)
	which_pair = 0

	# get individual sentences within pair
	for cond, sentence in enumerate(sentencepair):

		# get arr of words; remove period from last word
		words = sentence.split()
		words[len(words)-1] = words[len(words)-1].strip(".")
		words_freq = []

		# get word's logprob value
		for word in words:
			words_freq.append(wordDict[word][1])

		# append mean freq across words to appropriate array
		if not cond:
			mv_freq.append(np.nanmean(words_freq))
		else:
			nmv_freq.append(np.nanmean(words_freq))

	mv_comb_freq.append(statistics.mean([mv_freq[count],nmv_freq[count]]))

#  write results to csv
pd.DataFrame({'item_num':mv_item_num,'mv_freq': mv_freq, 'nmv_freq': nmv_freq, 'mv_comb_freq': mv_comb_freq}).to_csv("mv_wordfreq_results.csv")


# # loc stim

# with open('wordDict.pickle', 'rb') as f:
# 	wordDict = pickle.load(f)

# s_freq = []

# # read in csv with sentences
# sentences = np.genfromtxt("curr_stim_loc_nc.csv",delimiter = ',',dtype=str,encoding='utf-8-sig')

# # each row is a pair of sentences
# for count,sent in enumerate(sentences):
	
# 	print("Working on row %d\n"%count)

# 	sentence = sent

# 	# get arr of words; remove period from last word
# 	words = sentence.split()
# 	words[len(words)-1] = words[len(words)-1].strip(".")
# 	words_freq = []

# 	# get word's logprob value
# 	for word in words:
# 		words_freq.append(wordDict[word][1])

# 	# append mean freq across words to appropriate array
# 	s_freq.append(np.nanmean(words_freq))

# #  write results to csv
# pd.DataFrame({'s_freq': s_freq}).to_csv("wordfreq_results_loc.csv")

