# Fundamental Period
### _Bibek Panthi <bpanthi977@gmail.com>_

This is a project to estimate the fundamental period of buildings. 
It takes into account the building geometry: number of stories, height of story, bay count, bay spans, structural members (columns, beams, slab, walls) dimension and the physical properies (modulus of rigidity, density) of members.

# How to use 
* Setup a Common Lisp environment. [Portacle](https://portacle.github.io/) is one option.
* Download this library to the `projects` or `local-projects` directory of quicklisp. 
* Load it: `(ql:quickload :fundamental-period)`
* Create building models and calculate fundamental period see `(test)` function (in `fundamental-period.lisp` file) for examples. 

[workbook](https://github.com/bpanthi977/fundamental-period-workbook) repo contains many other examples, and pre-generated data files (in csv format). 

