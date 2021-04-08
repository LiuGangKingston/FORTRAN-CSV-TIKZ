# FORTRAN-CSV-TIKZ
To use FORTRAN computing and Tikz drawing powers together. 

     Gang Liu (gl.cell@outlook, http://orcid.org/0000-0003-1575-9290)
          and
     Shiwei Huang (huang937@gmail.com)
     
It is absolutely a good idea to combine FORTRAN computing and Tikz drawing powers. To do this, we may use FORTRAN to perform all key computations, then output all needed results into CSV files. Since Latex/Tikz can read CSV data files, it can draw figures accordingly. So CSV plays a key role in this scheme. Coding FORTRAN for generating CSV files should be done very very carefully. 

Here we will use the "CSVSIMPLE" package. Since it supports the option "[head to column names]" in the command "\csvreader", the CSV files are better generated with all names for the data columns as the first line. As "CSVSIMPLE" documented, any pair of neighbor names must be separated by the comma symbol ",", no other character is permitted in the first line. Once it is generated properly, the command

\csvreader[head to column names]{THE CSV FILE}{}{
...
}

in a tex file can read it, and automatically get macros in form of "\COLUMNNAME" for all comlumn data. Another caution is to avoid "," at the end of any line of the data file. 

