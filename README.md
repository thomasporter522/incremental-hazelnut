# Incremental Hazelnut

Todo: Marking 

- Each markable node needs a reference to a mark bit. When displaying, display with ! if marked. 
- Each mark needs to consist of a condition + a reference to a mark bit. 
- Each edit action E[D[es]] -> E[D'[es]] needs to traverse the mark trees of E's info and es's infos, evaluate the conditions given the new info, and set the bits. 
