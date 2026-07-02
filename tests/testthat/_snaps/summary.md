# summary

    Code
      summary(res)
    Message
      Fuzzed 1 function on 2 inputs:
    Output
            
             FAIL WARN SKIP OK
        list    0    0    0  2
      
      [ FAIL 0 | WARN 0 | SKIP 0 | OK 2 ]

---

    Code
      summary(res, tabulate = FALSE)
    Message
      Fuzzed 1 function on 2 inputs:
    Output
      
      [ FAIL 0 | WARN 0 | SKIP 0 | OK 2 ]

# print

    Code
      print(res)
    Message
      v    You didn't get caught by the fuzz!
    Output
      
       [ FAIL 0 | WARN 0 | SKIP 0 | OK 2 ] 

---

    Code
      print(res)
    Message
      x      CAUGHT BY THE FUZZ!    
      
      -- Test input [[2]]: 1:3, NA 
    Output
         median  FAIL  missing value where TRUE/FALSE needed
      
       [ FAIL 1 | WARN 0 | SKIP 2 | OK 3 ] 

---

    Code
      print(res, show = "all")
    Message
      x      CAUGHT BY THE FUZZ!    
      
      -- Test input [[1]]: NA, TRUE 
    Output
           list    OK  
         median    OK  
       Sys.date  SKIP  Object not found in the global namespace
    Message
      
      -- Test input [[2]]: 1:3, NA 
    Output
           list    OK  
         median  FAIL  missing value where TRUE/FALSE needed
       Sys.date  SKIP  Object not found in the global namespace
      
       [ FAIL 1 | WARN 0 | SKIP 2 | OK 3 ] 

---

    Code
      print(res, show = "skip")
    Message
      x      CAUGHT BY THE FUZZ!    
      
      -- Test input [[1]]: NA, TRUE 
    Output
       Sys.date  SKIP  Object not found in the global namespace
    Message
      
      -- Test input [[2]]: 1:3, NA 
    Output
       Sys.date  SKIP  Object not found in the global namespace
      
       [ FAIL 1 | WARN 0 | SKIP 2 | OK 3 ] 

---

    Code
      print(res, show = "none")
    Output
      [ FAIL 1 | WARN 0 | SKIP 2 | OK 3 ] 

