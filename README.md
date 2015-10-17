# seqparse - Command line tool for Eden-TraceLab

## Installation:

Eden-Tracelab is developed to work with the latest Eden compiler (September
2015) which is available at 
http://www.mathematik.uni-marburg.de/~eden/?content=down_eden&navi=down

The parser and web-backend can be compiled with cabal.
Both cabal projects should be compiled within a sandbox, to avoid cabal hell.

> cabal configure 
> cabal install --dependencies-only
> cabal build

For the parser to succesfully compile and run, libfastconvert (which is 
located in parser/lib) has to be on the include path. Compile it using 
make, and place it on your include path (e.g /usr/include), or pass its 
location to ghc.

## Usage 

Before starting the program, create a postgres database with the required schema. 
The according sql can be found [here](). Put the pq connection string specifying the database 
credentials in a configuration file called pq.conf.

Then, extract the *.parevents file you want to analyze into a directory, and execute the program: 
./seqparse path_to_directory_containing_unzipped_parevents_file
