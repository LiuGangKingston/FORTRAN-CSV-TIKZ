# FORTRAN-CSV-TIKZ
To use FORTRAN computing and Tikz drawing powers together. 

     Gang Liu (gl.cell@outlook, http://orcid.org/0000-0003-1575-9290)
          and
     Shiwei Huang (huang937@gmail.com)
     
It is absolutely a good idea to combine FORTRAN computing and Tikz drawing powers. To do this, we may use FORTRAN to perform all key computations, then output all needed results into CSV files. Since Latex/Tikz can read CSV data files, it can use the data to draw figures accordingly. So CSV plays a key role in this scheme. However, coding FORTRAN for generating CSV files should be done very very carefully. 

Here we will use the "CSVSIMPLE" package. Since it supports the option "[head to column names]" in the command "\csvreader", the CSV files are better generated with all names for the data columns as the first line. As "CSVSIMPLE" documented, any pair of neighbor names must be separated by the comma symbol ",", no other character (even a space) is permitted in the first line. Once it is generated properly, the command

\csvreader[head to column names]{THE CSV FILE}  {}  {
... DRAW ANYTHING HERE ...
}

in a tex file can read it, and automatically get macros in form of "\COLUMNNAME" for all comlumn data. Another caution is to avoid "," at the end of any line of the data file. In fact, the above command also invokes an iteration: to apply data line by line to perform the corresponding work of "{... DRAW ANYTHING HERE ...}". 

As in our example01, two CSV files will be generated: 

    open(31, file='setup.scalars.csv')
    open(32, file='iterated.alldata.csv')

The first will contain data computed only one-time for figure setup. The second one contains all itereated data for drawing more than one similar things in the example. To avoid problems, any data needed but generated by the FORTRAN code in the command
\csvreader[head to column names]{iterated.alldata.csv}  {}  {
... DRAW ANYTHING HERE ...
}
are outputted to the "iterated.alldata.csv" file, INCLUDING THOSE WHICH WERE ALREADY OUTPUTTED INTO THE FILE "setup.scalars.csv" . 

In version 2.0, more routines are added to the module, which is also renamed. The new routines make it easy to split a large amount of data into a group of CSV files.

