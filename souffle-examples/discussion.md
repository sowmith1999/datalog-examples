## Negation in Datalog
```souffle
path(from, to, acc + w), path_list(from, to) 
        :- path(from, mid, acc), edge(mid, to, w), !path_list(from, to).
```
- Souffle complains about the query having cyclic negation, I don't understand, I have a relation, I look if a fact exists, if it doesn't exist, I try to add it to the relation, Is that not what is happening here in essence.
```bash
sowmith@ogygia ~/d/p/souffle-examples> souffle -F./input -D./output ./src/sssp.dl
Error: Unable to stratify relation(s) {path,path_list}
Relation path_list in file sssp.dl at line 12
.decl path_list(src:number, to:number)
------^--------------------------------
has cyclic negation in file sssp.dl at line 19
        :- path(from, mid, acc), edge(mid, to, w), !path_list(from, to).
----------------------------------------------------^--------------------
1 errors generated, evaluation aborted
```
- I have no idea how negation works, Have to read Chapter 15 from Alice Book
