#from PDF version
#put new.pdf in downloads - the pdf version of the article
#output to JSStemp on desktop

import os
import pdfminer
import re
import sys
from collections import Counter

#1 Create text file from pdf

#command line e.g. python JSSpython.py 1413 pre #[or post] from the folder containg this script

jssnum = sys.argv[1]

newfile = "JSS_" + jssnum+ ".txt"
os.system("pdf2txt.py -o ~/Desktop/JSStemp/" + newfile + " ~/Downloads/new.pdf" )

#2 Create draft of screening

preorpost = sys.argv[2]
screen_name = "/Users/jac/Dropbox/Jessica_Josh/JSS/JSS " + jssnum + " " + preorpost + " comments DRAFT.txt"

screen = open(screen_name, "w")

screen.write("JSS "+jssnum+ ":")


#grab pdf -> text output
JSStext_path =  "/Users/jac/Desktop/JSStemp/JSS_"+jssnum+".txt"
JSStext_file = open(JSStext_path,"r")
lines = JSStext_file.read()

titleout = re.search('(?<=http://www.jstatsoft.org/)(.\n.*?){20}',lines,re.DOTALL)
titleout2 = re.search('.*(?=Abstract)',titleout.group(0),re.DOTALL | re.IGNORECASE)

screen.write(titleout2.group(0))

screen.write("---------------------------------------------------------\n"+
             "For further instruction on JSS style requirements please see the JSS style manual (in particular section 2.1 Style Checklist) at http://www.jstatsoft.org/downloads/JSSstyle.zip\n\n"+
             "Also see FAQ at: http://www.jstatsoft.org/style\n\n"+
             "For further references please see RECENT JSS papers for detailed documentation and examples.\n"+
             "---------------------------------------------------------\n\n"+
             "From the editorial team:\n\n")

if(preorpost=="pre"):
    screen.write("o We feel your submission is a code snippet rather than a full article. Please use the code snippet template with your resubmission.\n\n")

screen.write("\nManuscript style comments:\n\n")

#ABSTRACT
abstract = re.search('Abstract.*(?=Keywords)', lines ,re.DOTALL | re.IGNORECASE)
#print abstract.group(0)
abstract2 = re.search('.*\([A-Z]{2,10}.*\)', abstract.group(0))
justabbrev = re.findall('[a-z]*[A-Z]{2,10}', abstract.group(0)) #might want to disallow words later
if (len(justabbrev)>0):
    ja = min(Counter(justabbrev))
    jb = Counter(justabbrev)[ja]
    if (jb<2):
        screen.write(str(Counter(justabbrev)))
        screen.write("\no Please only introduce an abbreviation within the abstract if it is needed again within the abstract. Otherwise, please introduce it within the body of the manuscript.")


#TITLE

titleout3 = re.search('(.\n.*?){4}',titleout.group(0),re.DOTALL)
title0 = titleout3.group(0)
titlestyle1 = re.search(': [a-z]{1}.{10}',title0)
titlestyle2 = re.search(' [a-z]{4,20} .* ',title0)

try:
    screen.write(str(titlestyle1.group(0))+"\n\no title in title style (do capitalize the first word after a colon)")
except:
    pass

try:
    screen.write(str(titlestyle2.group(0))+"\n\no title in title style (all principal words should be capitalized)\n")
except:
    pass

#SECTION titles

sectionout = re.findall('[\\n][1-9]\. [a-zA-z]+\ [A-Z][^A-Z].*',lines)
subsectionout = re.findall('[\\n][1-9]\.[1-9]\.\ [a-zA-z]+\ .*[A-Z].*',lines)

if len(sectionout) > 0 or len(subsectionout)>0:
    screen.write("\n\no \section, \subsection, etc. should be in sentence style (http://www.jstatsoft.org/style#q2), e.g.,\n")
    for elem in sectionout:
        screen.write(elem.lstrip("\n"))
        screen.write("\n")
    for elem in subsectionout:
        screen.write(elem.lstrip("\n"))
        screen.write("\n")

#SUBSECTION

subsection = re.findall('[sS]ubsection.*',lines)
if len(subsection)>0:
    screen.write("\n\n\no For referring to subsections, do not use Subsection x.y, just Section x.y.\n")
    screen.write(str(subsection))

#CODE

carrot = re.findall('\n[a-zA-QS-Z]*>.*',lines)
comment = re.findall('#.*',lines)
isR = re.findall('\ R\ ',lines)
if len(isR)>5:
    if len(carrot)>0:
        screen.write("\n\no Please use \"R>\" as the command prompt, rather than \">\".\n")
        i = 1
        while i < min(10,len(carrot)):
            screen.write(carrot[i])
            screen.write("\n")
            i+=1

    if len(comment)>0:
        screen.write("\n\no The code presented in the manuscript should not contain comments within the verbatim code. Instead the comments should be made in the normal LaTeX text.")
    screen.write("\n\no For the code layout in R publications, we typically distinguish input/output using Sinput/Soutput (or equivalently CodeInput/CodeOutput). Unless there are special reasons to format it differently, the input should use the text width (up to 76 or 77 characters) and be indented by two spaces, e.g.,\n\nbegin{Sinput}\n\tR> example_model <- lm(response ~ variable1 + variable2 + variable3, \n+    weights = w, data = mydata)\n\end{Sinput}\n\n")



space = re.findall('>.*\,[^\ ]|>.*[^\ ][\+-=][^\ ]|>.*[^\ ][\+|-|\=]|>.*[+-=][^\ ]',lines)
if len(space)>0:
    i = 1
    while i < min(10,len(space)):
        screen.write(space[i])
        screen.write("\n")
        i+=1
    screen.write("\n\no Code should have enough spaces to facilitate reading.  Please include spaces before and after operators and after commas (unless spaces have syntactical meaning).")

datalib = re.findall('\ library\([^\"].*|\ data\([^\"].*',lines)
if len(datalib)>0:
    screen.write("\n\no For R-related manuscripts: The first argument of data() and library() should always be quoted, e.g., library(\"foo\").\n")
    screen.write(str(datalib))

#EG,IE

eg = re.findall('e\.g\.(?!\,).*|i\.e\.(?!\,).*',lines)

try:
    screen.write( "\n\no If using \"e.g.\" and \"i.e.\" add a comma after the period to keep LaTeX from interpreting them as the end of a sentence, i.e.: \"e.g., \" and \"i.e., \".\n\t" + str(eg))

except:
    pass


#NOT-AUTOMATED

screen.write("\n\no The rule for capitalizing the starting letters of Figure, Section and Table is as follows: If you are referring to a particular figure/section/table then capitalize the first letter, otherwise use a lower-case first letter. For example, something shown in Section 4 vs. there are three sections in this paper.")

#CAPTIONS

screen.write("\n\no Figures, tables and equations should be marked with a \label and referred to by \\ref, e.g., Figure~\\ref{...}.")
                     
screen.write("\n\no All captions should appear below the corresponding figure/table. The captions should be in sentence style and end with a period.  No additional formatting (such as \emph, \bf or \it) should be used for the caption.")

screen.write("\n\no All table row/column headers should also be in sentence style. There should not be further footnote-style annotations in tables; these should all be placed in the caption.")

screen.write("\n\no Equations should be marked with a \\label and referred to by either\n\tEquation~\\ref{...} (with capitalization, without parentheses)\n\t\tor\n\t(\\ref({...})\nwith the former being preferred if the number of equation references is not too large.")
            
screen.write("\n\no Code should preferably be presented in the usual text flow.")

primes = re.findall('cid:48',lines)
transp = re.findall('cid:62',lines)
screen.write("\n")
screen.write(str(primes))
screen.write("\n")
screen.write(str(transp))
screen.write("\n\no\\top should be used as the transpose symbol, e.g., X^\\top.")

screen.write("\n\no In all cases, code input/output must fit within the normal text width of the manuscript.  Thus, code input should have appropriate line breaks and code output should preferably be generated with a suitable width (or otherwise edited).")

screen.write("\n\no For bullet lists/itemized lists please use either a comma, semi-colon, or period at the end of each item, for example")

abbrev = re.findall('[A-Z]\.[A-Z]\.[A-Z]\.',lines)
if len(abbrev)>0:
    screen.write("\n\n")
    screen.write(str(abbrev))

screen.write("\n\no Abbreviations should be spelled in upper-case letters without additional formatting (i.e., without periods, without small caps, italics, etc.).  All abbreviations should be introduced with their expansion where the expansion should not be capitalized.")

screen.write("\n\no Do not use additional formatting for specific words unless explicitly required by the JSS style guide, e.g.,")

screen.write("\n\no As a reminder, please make sure that:\n\t- \\proglang, \\pkg and \\code have been used for highlighting throughout the paper (including titles and references), except where explicitly escaped.")

#References
screen.write("\n\n\nReferences:\n\n")

statistician = re.search('(?<!The).American Statistician.*',lines)
try:
    screen.write( "\no The American Statistician (not: American Statistician)\n" + "\t" + springer.group(0)+ "\n" )
except:
    pass

annals = re.search('(?<!The).Annals of Statistics.*',lines)
try:
    screen.write( "\no The Annals of Statistics (not: Annals of Statistics)\n" + "\t" + annals.group(0)+ "\n" )
except:
    pass

mathannals = re.search('(?<!The).Annals of Mathematical Statistics.*',lines)
try:
    screen.write("\no The Annals of Mathematical Statistics (not: Annals of Mathematical Statistics)\n" + "\t" + mathannals.group(0)+ "\n")
except:
    pass

royal = re.search('.*Royal Statistical Society.*[sS]eries.*?[A-D]+?',lines)

try:
    screen.write( "\no Journal of the Royal Statistical Society " + royal.group(0)[-1] + " (not: Journal of the Royal Statistical Society, Series " +  royal.group(0)[-1] + ")\n" + "\t" + royal.group(0)+ "\n" )
except:
    pass

try:
    screen.write( royal.group(0) )
except:
    pass

springer = re.search('.*Springer(?!-Verlag).*',lines)
try:
    screen.write( "\no Springer-Verlag (not: Springer)\n" + "\t" + springer.group(0)+ "\n" )
except:
    pass

wiley = re.search('(?<!John )Wiley.',lines)
wiley2 = re.search('Wiley and',lines)
wiley3 = re.search('.*Wiley.*Sons.*Inc.*',lines)
try:
    screen.write( "\no John Wiley & Sons (not: Wiley, John Wiley & Sons Inc.)\n" + "\t" + wiley.group(0)+ "\n" )
except:
    pass
try:
    screen.write( "\no John Wiley & Sons (not: Wiley, John Wiley & Sons Inc.)\n" + "\t" + wiley2.group(0)+ "\n" )
except:
    pass
try:
    screen.write( "\no John Wiley & Sons (not: Wiley, John Wiley & Sons Inc.)\n" + "\t" + wiley3.group(0)+ "\n" )
except:
    pass

fortran = re.search('.*FORTRAN.*|.*fortran.*',lines)
try:
    screen.write( "\no Fortran (not: FORTRAN)\n" + "\t" + fortran.group(0) + "\n" )
except:
    pass

java = re.search('.*JAVA.*|.*java.*',lines)
try:
    screen.write( "\no Java (not: JAVA, java)\n" + "\t" + java.group(0) + "\n" )
except:
    pass

matlab = re.search('.*Matlab.*|.*matlab.*',lines)

try:
    screen.write( "\no MATLAB (not: Matlab, matlab)\n" + "\t" + matlab.group(0) + "\n" )
except:
    pass

splus = re.search('.*Splus.*|.*S-Plus.*',lines)
try:
    screen.write( "\no S-PLUS (not: Splus, S-Plus)\n" + "\t" + splus.group(0) + "\n" )
except:
    pass

if preorpost == "pre":
    screen.write("\n\no Please make sure that all software packages are \cite{}'d properly.\no All references should be in title style.\no See FAQ for specific reference instructions.")

if preorpost == "post":
    screen.write("\n\no As a reminder,\n\t- Please make sure that all software packages are \cite{}'d properly.\n\n\t- All references should be in title style.\n\n\t- See FAQ for specific reference instructions.")

if preorpost == "pre":
    screen.write("\n\n\nCode:\n\no Please make sure that the files needed to replicate all code/examples within the manuscript are included in a standalone replication script.")

if preorpost == "post":
    screen.write("\n\n\nCode:\n\no As a reminder, please make sure that the files needed to replicate all code/examples within the manuscript are included in a standalone replication script.")




screen.close()

#Useful notes from Achim's email 10/2/14 (Regarding special issue)
#in R publications, we typically distinguish input/output using
#Sinput/Soutput (or equivalently CodeInput/CodeOutput). Unless there are
#special reasons to format it differently, the input should use the text
#width (up to 76 or 77 characters) and be indented by two spaces, e.g.

#\begin{Sinput}
#R> example_model <- lm(response ~ variable1 + variable2 + variable3,
#                       +    weights = w, data = mydata)
#\end{Sinput}
