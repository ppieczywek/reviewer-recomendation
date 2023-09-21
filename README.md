# reviewer-recomendation

### Introduction
One of the most critical roles of a scientific journal editor is to find potential reviewers for submitted papers. With the current volume of published material, this is extremely difficult. Often the selection is made based on suggestions from authors, or from the journal's database. Scientific journals build reviewer databases from authors of previously published papers, among others. The selection of a reviewer is made on the basis of a set of keywords assigned to the submitted work. However, such a selection is often misguided. In this repository, a different approach is presented, allowing rapid construction of the reviewer database and more accurate matching of the reviewer to the reviewed work.


### Reviewer data 
The approach is based on a text analysis technique and the bag-of-words concept. The database of reviewers is built on the basis of publicly available scientific publications and conference materials. Each such document includes a list of authors and their email address. For each document, a bag of the most frequently occurring words in it is built, excluding irrelevant words. These words have assigned probabilities of occurrence. The author of the publication is then pushed into the reviewer database, along with contact information and an assigned set of keywords extracted from the publication.


### Reviewer selection
Reviewer selection involves extraction of a bag of the most popular words for the reviewed work. Then, the bag of words is compared with each potential reviewer's set of keywords.The degree of reviewer match is determined by the number of keywords matched and the sum of their probabilities of occurrence for a given reviewer.


### Repository scripts
The scripts posted in the repository illustrate the process of reviewer selection. The "helper_functions.r" file contains the functions needed to extract keywords, a list of authors and email addresses from a PDF file of a scientific publication. This data is used to build a list of reviewers.

The demonstration is done with an example of an already built reviewer database. The database is storred in two CSV files. In each file, signle row corresponds to the author of a given document, for which an email address was provided. Further columns contain keywords (80 words, "reviewer_keywords.csv") and the frequency of their occurrence ("reviewer_scores.csv"), with the total number of words in a given document. The database was created based on conference proceedings (CIGR2015) and publications published in International Agrophysics (http://www.international-agrophysics.org/). The reviewers' data has been anonymized.
The "find_reviewer.r" script shows how to create a table of reviewers from CSV files, calculate the probabilities of keywords for reviewers, and how to do reviewer scoring for a sample publication.